library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)

# Open data ----
setwd ("C:\Users\yuval\Desktop\GenderDim_Analysis\Data")
dataFrom <- '20180725'
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

# Clean brms data ----
# Keep only trials with good animation
brms <- brms[bProblem == 0 & sProblem < 5]

trialCount <- brms[, .(trials = .N), by = uniqueid]
trialCount <- trialCount[trials >= 160]
brms <- brms[uniqueid %in% trialCount$uniqueid]

# Keep only correct trials
brms <- brms[acc == 1]

# Exclude short trials
brms <- brms[rt > 200]

# Exclude long trials
brms <- brms[rt < 15000]

ggplot(brms, aes(x = rt)) +
  geom_histogram(bins = 50) +
  facet_wrap('uniqueid', scales = 'free_x')

# Exclude outlier trials per subject
brms[, zrt := scale(rt), by = uniqueid]
brms <- brms[abs(zrt) < 3]

ggplot(brms, aes(x = rt)) +
  geom_histogram(bins = 50) +
  facet_wrap('uniqueid', scales = 'free_x')

# Plot BTs ----
mBT <- brms[, .(BT = mean(rt)), by = uniqueid]
ggplot(mBT, aes(x = BT)) +
  geom_histogram(bins = 15)

mBT <- merge(mBT, dems)

ggplot(mBT, aes(x = age, y = BT)) +
  geom_point()

cor.test(mBT$BT, mBT$age)

ggplot(mBT[, .(BT = mean(BT),
               se = sd(BT) / sqrt(.N)), 
           by = gender], aes(x = gender, y = BT, ymin = BT - se, ymax = BT + se)) +
  geom_pointrange()

t.test(BT ~ gender, mBT)

# check that there are enough trials for each face


#correlation between BT and dominance/trustworthinessin each of the four groups


#regression.(check that the BT mean and median are similiar first)


# find the priority dimension for each participants and stimuli sex
