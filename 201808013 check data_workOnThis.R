library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)

# Open data ----
setwd ("C:/Users/yuval/Desktop/GenderDim_Analysis/Data")
dataFrom <- '20180725'
brms <- fread(paste('../Data/', dataFrom, 'brms.csv', sep= ''))
quest <- fread(paste('../Data/', dataFrom, 'questionnaire.csv', sep= ''))
event <- fread(paste('../Data/', dataFrom, 'eventdata.csv', sep= ''))
jsevent <- fread(paste('../Data/', dataFrom, 'jseventdata.csv', sep= ''))
facetraits <- fread(paste('../Data/', '300FacesTraitPCs.csv', sep= ''))




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
                 politics_stemcells = fromJSON(responses[question == 'Stem Cell'])$Q0,
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

dems[, z_death_penalty := scale(-1 * politics_death_penalty)]
dems[, z_environment := scale(-1 * politics_environment)]
dems[, z_iraq := scale(politics_iraq)]
dems[, z_gays := scale(politics_gays)]
dems[, z_guns := scale(politics_guns)]
dems[, z_stemcells := scale(politics_stemcells)]
dems[, z_abortion := scale(politics_abortion)]
dems[, z_affirmative_action := scale(politics_affirmative_action)]

dems$politics_overall_avg <- dems[, .((politics_iraq + politics_gays + politics_guns + politics_stemcells + politics_abortion +
                                politics_affirmative_action - politics_death_penalty - politics_environment) / 8)]

dems$politics_z_avg <- dems[, .((z_iraq + z_gays + z_guns + z_stemcells + z_abortion +
                                         z_affirmative_action + z_death_penalty + z_environment) / 8)]

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

brms[, zrt := scale(rt), by = uniqueid]  #ask yaniv: should i scale again after removing extreme z scores?
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
  geom_point() +
  geom_smooth(method='lm')

  cor.test(mBT$BT, mBT$age)


ggplot(mBT, aes(x = politics_z_avg, y = BT)) +
  geom_point() +
  geom_smooth(method='lm')

  cor.test(mBT$BT, mBT$politics_z_avg)


ggplot(mBT, aes(x = age, y = politics_z_avg)) +
  geom_point() +
  geom_smooth(method='lm')
  
  cor.test(mBT$age, mBT$politics_z_avg)


ggplot(mBT[, .(BT = mean(BT),
               se = sd(BT) / sqrt(.N)), 
           by = gender], aes(x = gender, y = BT, ymin = BT - se, ymax = BT + se)) +
  geom_pointrange()

t.test(BT ~ gender, mBT)

# Plot Faces ----

# check that there are enough trials for each face
stimuluscount <- brms[, .(trials = .N), by = stimulus]
stimuluscount <- stimuluscount[order(trials)]

ggplot(stimuluscount, aes(x = trials)) +
  geom_histogram(bins = 30)


#make 'stimuli' D.T for faces. **ask yaniv if there is a need to make the collumns factors with levels.**
stimuli <- brms[,.(mean_BT = mean(rt)), by = stimulus]
mZT <- brms[,.(mean_Z = mean(zrt)), by = stimulus]
stimuli <- merge(stimuli, mZT)
stimuli <- merge(stimuli, stimuluscount)
stimuli <- stimuli[order(mean_BT)]

ggplot(stimuli, aes(x = mean_BT)) +
  geom_histogram(bins = 50)  #how many faces have each mean_BT?

stimuli_gender <- brms[,.(stimulus_id = substring(stimulus,18,21)), by = stimulus]   #add id num for each face
stimuli_gender <- stimuli_gender[, .(stim_gender = substring(stimulus,18,18), stimulus_id), by = stimulus]  #add stim_gender

stimuli <- merge(stimuli_gender, stimuli)


stimuli$stim_gender <- as.factor(stimuli$stim_gender)
stimuli$stimulus_id <- as.factor(stimuli$stimulus_id)

#check for global differences between male and female stimuli
ggplot(stimuli[, .(mean_BT = mean(mean_BT),
               se = sd(mean_BT) / sqrt(.N)), 
           by = stim_gender], aes(x = stim_gender, y = mean_BT, ymin = mean_BT - se, ymax = mean_BT + se)) +
  geom_pointrange()

t.test(mean_BT ~ stim_gender, stimuli)

#face traits
facetraits <- facetraits[,.(stimulus_id = substring(Stimulus, 1,4), Valence, Power, stim_gender)]  #**change when new data arrives
facetraits$stim_gender <- as.factor(facetraits$stim_gender)
facetraits$stimulus_id <- as.factor(facetraits$stimulus_id)

stimuli <- merge(stimuli, facetraits, by = c("stimulus_id", "stim_gender"), all.x = TRUE)
stimuli[,"stimulus":=NULL]

#global correlation between BT and dominance/trustworthiness
ggplot(stimuli, aes(x = Power, y = mean_BT)) +
  geom_point() +
  geom_smooth(method='lm')


cor.test(stimuli$mean_BT, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = mean_BT)) +
  geom_point() +
  geom_smooth(method='lm')


cor.test(stimuli$mean_BT, stimuli$Valence)

#by group correlation between BT and dominance/trustworthiness

brms <- merge(brms, dems[ , c("uniqueid", "gender")], by = "uniqueid") #add participents gender to brms

    #fXf
fXf_brms <- brms[gender == "Female" & stim_gender == "f",]
fXf_stimuli <- fXf_brms[,.(fXf_mBT = mean(rt)), by = stimulus]
fXf_mZ <- fXf_brms[,.(fXf_mean_Z = mean(zrt)), by = stimulus]
fXf_stimuli <- merge(fXf_stimuli, fXf_mZ)
fXf_stimuli[, stimulus_id := factor(substring(stimulus, 18,21))]
stimuli <- merge(stimuli, fXf_stimuli[ , c("stimulus_id", "fXf_mBT","fXf_mean_Z")], by = "stimulus_id", all.x = TRUE)

ggplot(stimuli, aes(x = Power, y = fXf_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXf_mBT, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = fXf_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXf_mBT, stimuli$Valence)

ggplot(stimuli, aes(x = Power, y = fXf_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXf_mean_Z, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = fXf_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXf_mean_Z, stimuli$Valence)

    #mXf
mXf_brms <- brms[gender == "Male" & stim_gender == "f",]
mXf_stimuli <- mXf_brms[,.(mXf_mBT = mean(rt)), by = stimulus]
mXf_mZ <- mXf_brms[,.(mXf_mean_Z = mean(zrt)), by = stimulus]
mXf_stimuli <- merge(mXf_stimuli, mXf_mZ)
mXf_stimuli[, stimulus_id := factor(substring(stimulus, 18,21))]
stimuli <- merge(stimuli, mXf_stimuli[ , c("stimulus_id", "mXf_mBT", "mXf_mean_Z")], by = "stimulus_id", all.x = TRUE)

ggplot(stimuli, aes(x = Power, y = mXf_mBT)) +
  geom_point() +
  geom_smooth(method='lm')

  cor.test(stimuli$mXf_mBT, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = mXf_mBT)) +
  geom_point() +
  geom_smooth(method='lm')

  cor.test(stimuli$mXf_mBT, stimuli$Valence)

ggplot(stimuli, aes(x = Power, y = mXf_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')

  cor.test(stimuli$mXf_mean_Z, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = mXf_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')

  cor.test(stimuli$mXf_mean_Z, stimuli$Valence)

    #mXm
mXm_brms <- brms[gender == "Male" & stim_gender == "m",]
mXm_stimuli <- mXm_brms[,.(mXm_mBT = mean(rt)), by = stimulus]
mXm_mZ <- mXm_brms[,.(mXm_mean_Z = mean(zrt)), by = stimulus]
mXm_stimuli <- merge(mXm_stimuli, mXm_mZ)
mXm_stimuli[, stimulus_id := factor(substring(stimulus, 18,21))]
stimuli <- merge(stimuli, mXm_stimuli[ , c("stimulus_id", "mXm_mBT", "mXm_mean_Z")], by = "stimulus_id", all.x = TRUE)

ggplot(stimuli, aes(x = Power, y = mXm_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$mXm_mBT, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = mXm_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$mXm_mBT, stimuli$Valence)

ggplot(stimuli, aes(x = Power, y = mXm_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$mXm_mean_Z, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = mXm_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$mXm_mean_Z, stimuli$Valence)

    #fXm
fXm_brms <- brms[gender == "Female" & stim_gender == "m",]
fXm_stimuli <- fXm_brms[,.(fXm_mBT = mean(rt)), by = stimulus]
fXm_mZ <- fXm_brms[,.(fXm_mean_Z = mean(zrt)), by = stimulus]
fXm_stimuli <- merge(fXm_stimuli, fXm_mZ)
fXm_stimuli[, stimulus_id := factor(substring(stimulus, 18,21))]
stimuli <- merge(stimuli, fXm_stimuli[ , c("stimulus_id", "fXm_mBT", "fXm_mean_Z")], by = "stimulus_id", all.x = TRUE)

ggplot(stimuli, aes(x = Power, y = fXm_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXm_mBT, stimuli$Power)

ggplot(stimuli, aes(x = Valence, y = fXm_mBT)) +
  geom_point() +
  geom_smooth(method='lm')


  cor.test(stimuli$fXm_mBT, stimuli$Valence)
 
ggplot(stimuli, aes(x = Power, y = fXm_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')

  
  cor.test(stimuli$fXm_mean_Z, stimuli$Power)
  
ggplot(stimuli, aes(x = Valence, y = fXm_mean_Z)) +
  geom_point() +
  geom_smooth(method='lm')

  
  cor.test(stimuli$fXm_mean_Z, stimuli$Valence)
  

# reverse correlation- find the priority dimension for each participants and stimuli sex
