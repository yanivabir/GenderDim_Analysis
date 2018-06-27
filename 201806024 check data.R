library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)

# Open data ----
setwd('/Users/yanivabir/Google Drive/Lab/GenderDim/GenderDim_Analysis')
dataFrom <- '20180624'
brms <- fread(paste('../Data/', dataFrom, 'brms.csv', sep= ''))
quest <- fread(paste('../Data/', dataFrom, 'questionnaire.csv', sep= ''))
event <- fread(paste('../Data/', dataFrom, 'eventdata.csv', sep= ''))
jsevent <- fread(paste('../Data/', dataFrom, 'jseventdata.csv', sep= ''))


# Prepare data ----
brms$uniqueid <- factor(brms$uniqueid)
brms[, stim_gender := factor(substring(stimulus, 18,18))]
summary(brms)

# Discard training trials
brms <- brms[!(is.na(trial))]

# Keep only subjects who completed the task
trialCount <- brms[, .(trials = .N), by = uniqueid]
trialCount <- trialCount[trials >= 200]
brms <- brms[uniqueid %in% trialCount$uniqueid]
quest <- quest[uniqueid %in% trialCount$uniqueid]
event <- event[uniqueid %in% trialCount$uniqueid]
jsevent <- jsevent[uniqueid %in% trialCount$uniqueid]

# Extract demographics ----
# Get only questions
dems <- subset(quest, grepl("survey", trial_type, fixed = TRUE))

# Fix for JSON parsing
dems$responses <- gsub('\"\"', '\"', dems$responses)
dems$responses <- gsub(':\"}', ':\"\"}', dems$responses)

# Parse JSON responses
dems <- dems[, .(age = as.numeric(fromJSON(responses[internal_node_id == '0.0-10.0'])$Q0),
                 attn_deficit = fromJSON(responses[internal_node_id == '0.0-10.0'])$Q1,
                 gender = fromJSON(responses[internal_node_id == '0.0-11.0'])$Q0,
                 hand = fromJSON(responses[internal_node_id == '0.0-11.0'])$Q1,
                 native = fromJSON(responses[internal_node_id == '0.0-11.0'])$Q2,
                 fluency = as.numeric(fromJSON(responses[internal_node_id == '0.0-12.0'])),
                 strategy = fromJSON(responses[internal_node_id == '0.0-13.0'])$Q0,
                 sexuality = fromJSON(responses[internal_node_id == '0.0-14.0'])$Q0,
                 attracted = fromJSON(responses[internal_node_id == '0.0-14.0'])$Q1,
                 driver = fromJSON(responses[internal_node_id == '0.0-24.0'])$Q0,
                 driving_ability_text = responses[internal_node_id == '0.0-25.0-0.0'],
                 accidents_driver_text = responses[internal_node_id == '0.0-25.0-1.0'],
                 accidents_pedestrian = as.numeric(fromJSON(responses[internal_node_id == '0.0-26.0'])$Q0),
                 politics_death_penalty = fromJSON(responses[question == 'Death penalty'])$Q0,
                 politics_environment = fromJSON(responses[question == 'Environment laws'])$Q0,
                 politics_iraq = fromJSON(responses[question == 'Iraq'])$Q0,
                 politics_gays = fromJSON(responses[question == 'Homosexuals'])$Q0,
                 politics_guns = fromJSON(responses[question == 'Gun control'])$Q0,
                 politics_stemcelss = fromJSON(responses[question == 'Stem Cell'])$Q0,
                 politics_abortion = fromJSON(responses[question == 'Abortion'])$Q0,
                 politics_affirmative_action = fromJSON(responses[question == 'Affirmative action'])$Q0), 
             by = .(uniqueid)]
dems[!(is.na(driving_ability_text)), c('driving_ability', 'accidents_driver') := 
       list(as.numeric(fromJSON(driving_ability_text)), as.numeric(fromJSON(accidents_driver_text))), 
     by = uniqueid]

dems$uniqueid <- factor(dems$uniqueid)
dems$hand <- factor(dems$hand)
dems$driver <- factor(dems$driver)
dems$gender <- factor(dems$gender)
dems$native <- factor(dems$native)
dems$sexuality <- factor(dems$sexuality)
dems$attracted <- factor(dems$attracted)
summary(dems)

ggplot(dems, aes(x = age)) +
  geom_histogram(bins = 15)

# Exclude by event data ----
# Look at focus loss
ps_focus <- subset(event, eventtype == 'focus' & uniqueid %in% brms$uniqueid)

# Recorded by psiturk
for (ii in 1:(nrow(ps_focus) - 1)){
  if (with(ps_focus, value[ii] == 'off')){
    if (with(ps_focus, value[ii + 1] == 'on' && uniqueid[ii] == uniqueid[ii+1])) {
      brms[uniqueid == ps_focus$uniqueid[ii] & trial_began >= ps_focus$timestamp[ii] & 
             trial_began + rt <= ps_focus$timestamp[ii+1],'ps_focus_problem'] <- T
    }else{
      brms[uniqueid == ps_focus$uniqueid[ii] & 
             trial_began >= ps_focus$timestamp[ii],'ps_focus_problem'] <- T
    }
  }
}

# Recorded by jsPsych
js_focus <- subset(jsevent, event == 'focus' | event == 'blur')
for (ii in 1:(nrow(js_focus) - 1)){
  if (with(js_focus, event[ii] == 'blur')){
    if (with(js_focus, event[ii + 1] == 'focus' && uniqueid[ii] == uniqueid[ii+1])) {
      brms[uniqueid == js_focus$uniqueid[ii] & trial_index >= js_focus$trial[ii] & 
             trial_index <= js_focus$trial[ii+1],'js_focus_problem'] <- T
    }else{
      brms[uniqueid == js_focus$uniqueid[ii] & 
             trial_index >= js_focus$trial[ii],'js_focus_problem'] <- T
    }
  }
}

brms[is.na(brms$js_focus_problem), 'js_focus_problem'] <- F
brms[is.na(brms$ps_focus_problem), 'ps_focus_problem'] <- F

# Remove trials
brms <- brms[!js_focus_problem & !ps_focus_problem]

# Clean brms data ----
# Keep only trials with good animation
brms <- brms[bProblem == 0 & sProblem < 5]

trialCount <- brms[, .(trials = .N), by = uniqueid]
trialCount <- trialCount[trials >= 160]
brms <- brms[uniqueid %in% trialCount$uniqueid]

# Accuracy per subject
Acc <- brms[, .(acc = mean(acc)), by = uniqueid]
Acc <- Acc[acc >= .9]
brms <- brms[uniqueid %in% Acc$uniqueid]

# Keep only correct trials
brms <- brms[acc == 1]

# Exclude short trials
brms <- brms[rt > 200]

# Exclude long trials
brms <- brms[rt < 15000]

# ggplot(brms, aes(x = rt)) +
#   geom_histogram(bins = 50) +
#   facet_wrap('uniqueid', scales = 'free_x')

# Exclude outlier trials per subject
brms[, zrt := scale(rt), by = uniqueid]
brms <- brms[abs(zrt) < 3]

# ggplot(brms, aes(x = rt)) +
#   geom_histogram(bins = 50) +
#   facet_wrap('uniqueid', scales = 'free_x')

# Plot BTs ----
mBT <- brms[, .(BT = mean(rt)), by = .(uniqueid, stim_gender)]
ggplot(mBT, aes(x = BT)) +
  geom_histogram(bins = 15)

# Remove outlier subjects
mBT[, ZBT := scale(BT)]
# mBT <- mBT[abs(ZBT) < 3]
mBT <- mBT[BT <= (quantile(BT, 0.75) + 1.5 * IQR(BT) & BT) >= (quantile(BT, 0.25) - 1.5 * IQR(BT))]
brms <- brms[uniqueid %in% mBT$uniqueid]

ggplot(mBT, aes(x = BT)) +
  geom_histogram(bins = 15) +
  xlab('BT (ms)') + ylab('# of Ps') + theme(text = element_text(size=20))


mBT <- merge(mBT, dems)

ggplot(mBT, aes(x = age, y = BT)) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() + ylab('BT (ms)') + xlab('Age') + theme(text = element_text(size=20))

cor.test(mBT$BT, mBT$age)

ggplot(mBT, aes(x = gender, y = BT)) +
  geom_point()

t.test(BT ~ gender, mBT[!(gender == 'Other')])
t.test(BT ~ stim_gender, mBT)

# Factorial ANOVA
mod <- ezANOVA(mBT,
               dv = BT,
               wid = uniqueid,
               between = .(gender, stim_gender),
               type = 3)
print(mod)

mBT[, stim_gender := ifelse(stim_gender == 'f', 'Female', 'Male')]
ggplot(mBT[, .(BT = mean(BT),
              se = sd(BT) / sqrt(.N)), by = .(stim_gender, gender)], 
       aes(x = stim_gender, y = BT, color = gender, ymin = BT - se, ymax = BT + se)) +
  geom_pointrange() + ylab('BT (ms)') + xlab('Face gender') +
  guides(color=guide_legend(title="Gender")) + theme(text = element_text(size=20))

# Trial over stimuli ----
tPerStim <- brms[, .(trials = .N), by = stimulus]


