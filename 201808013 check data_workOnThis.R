library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(Hmisc)


# Open data ----
setwd ("C:/Users/User/Desktop/GenderDim_Analysis/Data") #lab pc
#setwd ("C:/Users/yuval/Desktop/GenderDim_Analysis") #laptop
dataFrom <- '20180725'
brms <- fread(paste(dataFrom, 'brms.csv', sep= ''))
quest <- fread(paste(dataFrom, 'questionnaire.csv', sep= ''))
event <- fread(paste(dataFrom, 'eventdata.csv', sep= ''))
jsevent <- fread(paste(dataFrom, 'jseventdata.csv', sep= ''))
facetraits <- fread(paste('300FacesTraitPCs.csv', sep= ''))

female_params <- fread(paste('300female_coord_allparams.csv', sep= ''))
male_params <- fread(paste('300male_coord_allparams.csv', sep= ''))
social_dims <- fread(paste('si-genders.csv', sep= ''))

yanivs_face_params <- fread(paste('oosterhof_todorov_300_faces_component_values.csv', sep= ''))
yanivs_face_params <- yanivs_face_params[,1:50]
yanivs_dims <- fread(paste('yaniv_dims_priority_trust_dom.csv', sep= ''))
yanivs_raw_dims <- fread(paste('yanivs_raw_social_dims.csv', sep= ''))

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
sd(dems$age, na.rm = TRUE) #age sd

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

#ggplot(brms, aes(x = rt)) +
#  geom_histogram(bins = 50) +
#  facet_wrap('uniqueid', scales = 'free_x')

# Exclude outlier trials per subject

brms[, zrt := scale(rt), by = uniqueid]  #ask yaniv: should i scale again after removing extreme z scores?
brms <- brms[abs(zrt) < 3]
#brms[, zrt := scale(rt), by = uniqueid] #another scale after removal- not used in this exp.

#ggplot(brms, aes(x = rt)) +
#  geom_histogram(bins = 50) +
#  facet_wrap('uniqueid', scales = 'free_x')

# Plot BTs ----

mBT <- brms[, .(BT = mean(rt)), by = uniqueid]
ggplot(mBT, aes(x = BT)) +
  geom_histogram(bins = 15)

mBT <- merge(mBT, dems)

ggplot(mBT, aes(x = age, y = BT)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(x = "participant age", y = "average BT")

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
  geom_pointrange(size = 1) +
  labs(x = "participants gender", y = "average BT", tag = "a")

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
levels(stimuli$stim_gender) <- c(levels(stimuli$stim_gender), "Female", "Male") 
stimuli$stim_gender[stimuli$stim_gender == "m"] <- "Male"
stimuli$stim_gender[stimuli$stim_gender == "f"] <- "Female"

ggplot(stimuli[, .(mean_BT = mean(mean_BT),
               se = sd(mean_BT) / sqrt(.N)), 
           by = stim_gender], aes(x = stim_gender, y = mean_BT, ymin = mean_BT - se, ymax = mean_BT + se)) +
  geom_pointrange(size = 1) +
  labs(x = "stimuli gender", y = "average BT", tag = "b")

stimuli$stim_gender[stimuli$stim_gender == "Male"] <- "m"
stimuli$stim_gender[stimuli$stim_gender == "Female"] <- "f"



t.test(mean_BT ~ stim_gender, stimuli)

#face traits
facetraits <- facetraits[,.(stimulus_id = substring(Stimulus, 1,4), Valence, Power, stim_gender)]  #**change when new data arrives
facetraits$stim_gender <- as.factor(facetraits$stim_gender)
facetraits$stimulus_id <- as.factor(facetraits$stimulus_id)

stimuli <- merge(stimuli, facetraits, by = c("stimulus_id", "stim_gender"), all.x = TRUE)
stimuli[,"stimulus":=NULL]

#global correlation between BT and dominance/trustworthiness
#ggplot(stimuli, aes(x = Power, y = mean_BT)) +
  #geom_point() +
  #geom_smooth(method='lm')


#cor.test(stimuli$mean_BT, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = mean_BT)) +
 #geom_point() +
 # geom_smooth(method='lm')


#cor.test(stimuli$mean_BT, stimuli$Valence)


#merge female and male stimuli to one df
stimuli$stim_gender <- NULL
stimuli_F <- stimuli[1:300]
  stimuli_F[, stimulus_id := factor(substring(stimulus_id, 2,4))]

stimuli_M <- stimuli[301:600]
  stimuli_M[, stimulus_id := factor(substring(stimulus_id, 2,4))]


stimuli <- merge(stimuli_F, stimuli_M, by = "stimulus_id")
colnames(stimuli) <- gsub('.x','.F',names(stimuli))
colnames(stimuli) <- gsub('.y','.M',names(stimuli))

#by group correlation between BT and dominance/trustworthiness

brms <- merge(brms, dems[ , c("uniqueid", "gender")], by = "uniqueid") #add participants gender to brms

    #fXf
fXf_brms <- brms[gender == "Female" & stim_gender == "f",]
fXf_stimuli <- fXf_brms[,.(fXf_mBT = mean(rt)), by = stimulus]
fXf_mZ <- fXf_brms[,.(fXf_mean_Z = mean(zrt)), by = stimulus]
fXf_stimuli <- merge(fXf_stimuli, fXf_mZ)
fXf_stimuli[, stimulus_id := factor(substring(stimulus, 19,21))]
stimuli <- merge(stimuli, fXf_stimuli[ , c("stimulus_id", "fXf_mBT","fXf_mean_Z")], by = "stimulus_id", all.x = TRUE)

#ggplot(stimuli, aes(x = Power, y = fXf_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$fXf_mBT, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = fXf_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$fXf_mBT, stimuli$Valence)

#ggplot(stimuli, aes(x = Power, y = fXf_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')


 # cor.test(stimuli$fXf_mean_Z, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = fXf_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$fXf_mean_Z, stimuli$Valence)

    #mXf
mXf_brms <- brms[gender == "Male" & stim_gender == "f",]
mXf_stimuli <- mXf_brms[,.(mXf_mBT = mean(rt)), by = stimulus]
mXf_mZ <- mXf_brms[,.(mXf_mean_Z = mean(zrt)), by = stimulus]
mXf_stimuli <- merge(mXf_stimuli, mXf_mZ)
mXf_stimuli[, stimulus_id := factor(substring(stimulus, 19,21))] 
stimuli <- merge(stimuli, mXf_stimuli[ , c("stimulus_id", "mXf_mBT", "mXf_mean_Z")], by = "stimulus_id", all.x = TRUE)

#ggplot(stimuli, aes(x = Power, y = mXf_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')

  #cor.test(stimuli$mXf_mBT, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = mXf_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')

  #cor.test(stimuli$mXf_mBT, stimuli$Valence)

#ggplot(stimuli, aes(x = Power, y = mXf_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')

  #cor.test(stimuli$mXf_mean_Z, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = mXf_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')

  #cor.test(stimuli$mXf_mean_Z, stimuli$Valence)

    #mXm
mXm_brms <- brms[gender == "Male" & stim_gender == "m",]
mXm_stimuli <- mXm_brms[,.(mXm_mBT = mean(rt)), by = stimulus]
mXm_mZ <- mXm_brms[,.(mXm_mean_Z = mean(zrt)), by = stimulus]
mXm_stimuli <- merge(mXm_stimuli, mXm_mZ)
mXm_stimuli[, stimulus_id := factor(substring(stimulus, 19,21))]
stimuli <- merge(stimuli, mXm_stimuli[ , c("stimulus_id", "mXm_mBT", "mXm_mean_Z")], by = "stimulus_id", all.x = TRUE)

#ggplot(stimuli, aes(x = Power, y = mXm_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$mXm_mBT, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = mXm_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$mXm_mBT, stimuli$Valence)

#ggplot(stimuli, aes(x = Power, y = mXm_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$mXm_mean_Z, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = mXm_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$mXm_mean_Z, stimuli$Valence)

    #fXm
fXm_brms <- brms[gender == "Female" & stim_gender == "m",]
fXm_stimuli <- fXm_brms[,.(fXm_mBT = mean(rt)), by = stimulus]
fXm_mZ <- fXm_brms[,.(fXm_mean_Z = mean(zrt)), by = stimulus]
fXm_stimuli <- merge(fXm_stimuli, fXm_mZ)
fXm_stimuli[, stimulus_id := factor(substring(stimulus, 19,21))]
stimuli <- merge(stimuli, fXm_stimuli[ , c("stimulus_id", "fXm_mBT", "fXm_mean_Z")], by = "stimulus_id", all.x = TRUE)

#ggplot(stimuli, aes(x = Power, y = fXm_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$fXm_mBT, stimuli$Power)

#ggplot(stimuli, aes(x = Valence, y = fXm_mBT)) +
 # geom_point() +
  #geom_smooth(method='lm')


  #cor.test(stimuli$fXm_mBT, stimuli$Valence)
 
#ggplot(stimuli, aes(x = Power, y = fXm_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')

  
 # cor.test(stimuli$fXm_mean_Z, stimuli$Power)
  
#ggplot(stimuli, aes(x = Valence, y = fXm_mean_Z)) +
 # geom_point() +
  #geom_smooth(method='lm')

  
  #cor.test(stimuli$fXm_mean_Z, stimuli$Valence)
  

# reverse correlation ----

female_params <- female_params[2:301,2:51]
male_params <- male_params[2:301,2:51]

### Define dimension extraction procedure as a function
extractDimension <- function(x, faces = faces, score_faces = score_faces, result = "dimension") {
  
  completeVector <- complete.cases(x)
  faces <- faces[completeVector]
  x <- x[completeVector]
  faces <- data.matrix(faces) # Make face data frame into a matrix easy to work with
  
  # Subtract the mean from Its
  x <- x - mean(x)
    # Create the weighted average
  Dim <- x %*% faces
  
  # Normalize
  Dim <- t(Dim / sqrt(sum(Dim^2)))
  
  # Return diemsnion, or dimension scores (computed as projection of each face on dimension)
  return(switch(result, scores = drop(faces %*% Dim), dimension = Dim))
  
}



fXm_dim <- extractDimension(stimuli[,fXm_mean_Z], male_params)
fXf_dim <- extractDimension(stimuli[,fXf_mean_Z], female_params)
mXm_dim <- extractDimension(stimuli[,mXm_mean_Z], male_params)
mXf_dim <- extractDimension(stimuli[,mXf_mean_Z], female_params)
bothXm_dim <- extractDimension(stimuli[,mean_Z.M], male_params)
bothXf_dim <- extractDimension(stimuli[,mean_Z.F], female_params)

fXm_dim_sc <- extractDimension(stimuli[,fXm_mean_Z], male_params, result = "scores")
fXf_dim_sc <- extractDimension(stimuli[,fXf_mean_Z], female_params, result = "scores")
mXm_dim_sc <- extractDimension(stimuli[,mXm_mean_Z], male_params, result = "scores")
mXf_dim_sc <- extractDimension(stimuli[,mXf_mean_Z], female_params, result = "scores")
bothXm_dim_sc <- extractDimension(stimuli[,mean_Z.M], male_params, result = "scores")
bothXf_dim_sc <- extractDimension(stimuli[,mean_Z.F], female_params, result = "scores")


### check correlation with dimensions

social_dims <- social_dims[,1:51]

trust_fXm <- social_dims[1,2:51]
dom_fXm <- social_dims[2,2:51]
trust_fXf <- social_dims[3,2:51]
dom_fXf <- social_dims[4,2:51]
trust_mXm <- social_dims[5,2:51]
dom_mXm <- social_dims[6,2:51]
trust_mXf <- social_dims[7,2:51]
dom_mXf <- social_dims[8,2:51]
trust_bothXm <- social_dims[9,2:51]
dom_bothXm <- social_dims[10,2:51]
trust_bothXf <- social_dims[11,2:51]
dom_bothXf <- social_dims[12,2:51]

#by groups
cor.test(as.numeric( dom_fXm), (fXm_dim))
cor.test(as.numeric( trust_fXm), (fXm_dim))
cor.test(as.numeric( dom_fXf), (fXf_dim))
cor.test(as.numeric( trust_fXf), (fXf_dim))
cor.test(as.numeric( dom_mXm), (mXm_dim))
cor.test(as.numeric( trust_mXm), (mXm_dim))
cor.test(as.numeric( dom_mXf), (mXf_dim))
cor.test(as.numeric( trust_mXf), (mXf_dim))

#by stimulus gender only
cor.test(as.numeric( dom_bothXm), (bothXm_dim))
cor.test(as.numeric( trust_bothXm), (bothXm_dim))
cor.test(as.numeric( dom_bothXf), (bothXf_dim))
cor.test(as.numeric( trust_bothXf), (bothXf_dim))

#check cor between priority dim and bt's
fXf_mZ <- fXf_mZ[order(stimulus)]
fXf_mZ <- cbind(fXf_mZ, fXf_dim_sc)
fXm_mZ <- fXm_mZ[order(stimulus)]
fXm_mZ <- cbind(fXm_mZ, fXm_dim_sc)
mXf_mZ <- mXf_mZ[order(stimulus)]
mXf_mZ <- cbind(mXf_mZ, mXf_dim_sc)
mXm_mZ <- mXm_mZ[order(stimulus)]
mXm_mZ <- cbind(mXm_mZ, mXm_dim_sc)
bothXm_mZ <- data.table(cbind(bothXm_dim_sc,bothXm_mean_Z = stimuli[,mean_Z.M ]))
bothXf_mZ <- data.table(cbind(bothXf_dim_sc,bothXf_mean_Z = stimuli[,mean_Z.F ]))


cor.test(fXf_dim_sc, fXf_mZ[,fXf_mean_Z])
ggplot(fXf_mZ, aes(x = fXf_mean_Z, y = fXf_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(fXm_dim_sc, fXm_mZ[,fXm_mean_Z])
ggplot(fXm_mZ, aes(x = fXm_mean_Z, y = fXm_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(mXf_dim_sc, mXf_mZ[,mXf_mean_Z])
ggplot(mXf_mZ, aes(x = mXf_mean_Z, y = mXf_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(mXm_dim_sc, mXm_mZ[,mXm_mean_Z])
ggplot(mXm_mZ, aes(x = mXm_mean_Z, y = mXm_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(bothXm_dim_sc, bothXm_mZ[,bothXm_mean_Z])
ggplot(bothXm_mZ, aes(x = bothXm_mean_Z, y = bothXm_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(bothXf_dim_sc, bothXf_mZ[,bothXf_mean_Z])
ggplot(bothXf_mZ, aes(x = bothXf_mean_Z, y = bothXf_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')


###check dimensions correlations----
yanivs_priority <- as.matrix(yanivs_dims[,2])
yanivs_trust <- as.matrix(yanivs_dims[,3])
yanivs_dom <- as.matrix(yanivs_dims[,4])
yanivs_raw_trust <- as.matrix(yanivs_raw_dims[,2])
yanivs_raw_dom <- as.matrix(yanivs_raw_dims[,3])

###check how yanivs priority dimension explaines scores in each group
yaniv_fXm_dim_sc <- extractDimension(stimuli[,fXm_mean_Z], faces = yanivs_face_params, male_params, result = "scores")
yaniv_mXm_dim_sc <- extractDimension(stimuli[,mXm_mean_Z], faces = yanivs_face_params, male_params, result = "scores")
yaniv_fXf_dim_sc <- extractDimension(stimuli[,fXf_mean_Z], faces = yanivs_face_params, female_params, result = "scores")
yaniv_mXf_dim_sc <- extractDimension(stimuli[,mXf_mean_Z], faces = yanivs_face_params, female_params, result = "scores")

cor.test(yaniv_fXm_dim_sc, fXm_mZ[,fXm_mean_Z])
ggplot(fXm_mZ, aes(x = fXm_mean_Z, y = yaniv_fXm_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')
cor.test(yaniv_mXm_dim_sc, mXm_mZ[,mXm_mean_Z])
ggplot(mXm_mZ, aes(x = mXm_mean_Z, y = yaniv_mXm_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')
cor.test(yaniv_fXf_dim_sc, fXf_mZ[,fXf_mean_Z])
ggplot(fXf_mZ, aes(x = fXf_mean_Z, y = yaniv_fXf_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')
cor.test(yaniv_mXf_dim_sc, mXf_mZ[,mXf_mean_Z])
ggplot(mXf_mZ, aes(x = mXf_mean_Z, y = yaniv_mXf_dim_sc)) +
  geom_point() +
  geom_smooth(method='lm')

###find cor between the 4 priority dimensions + yanivs dimension
priority_dims_merged <- cbind(fXf_dim, fXm_dim, mXf_dim, mXm_dim, bothXm_dim, yanivs_priority)
colnames(priority_dims_merged) <- c('fXf','fXm', 'mXf', 'mXm','bothXm', 'yaniv')
rcorr(priority_dims_merged)

###find cor between the different social dimensions + yanivs
doms_merged <- cbind(as.numeric(dom_fXf),as.numeric(dom_fXm), as.numeric(dom_mXf), as.numeric(dom_mXm), as.numeric(dom_bothXm), yanivs_dom, yanivs_raw_dom)
colnames(doms_merged) <- c('fXf','fXm', 'mXf', 'mXm', 'bothXm', 'yaniv', 'yaniv raw')
rcorr(doms_merged)

trusts_merged <- cbind(as.numeric(trust_fXf),as.numeric(trust_fXm), as.numeric(trust_mXf),as.numeric(trust_mXm), as.numeric(trust_bothXm), yanivs_trust, yanivs_raw_trust)
colnames(trusts_merged) <- c('fXf','fXm', 'mXf', 'mXm', 'bothXm', 'yaniv', 'yaniv raw')
rcorr(trusts_merged)

###find cor between yaniv priority dimension and my dominance and trustworthiness dimensions
temp1 <- cor.test(as.numeric( dom_fXm), (yanivs_priority))
temp2 <- cor.test(as.numeric( trust_fXm), (yanivs_priority))
temp3 <- cor.test(as.numeric( dom_fXf), (yanivs_priority))
temp4 <- cor.test(as.numeric( trust_fXf), (yanivs_priority))
temp5 <- cor.test(as.numeric( dom_mXm), (yanivs_priority))
temp6 <- cor.test(as.numeric( trust_mXm), (yanivs_priority))
temp7 <- cor.test(as.numeric( dom_mXf), (yanivs_priority))
temp8 <- cor.test(as.numeric( trust_mXf), (yanivs_priority))

yaniv_w_yuval_socials <- data.table(
  'social Dimension' = c('dominance', 'trust'),
  fxm = c(temp1$estimate,temp2$estimate),
  fxf = c(temp3$estimate,temp4$estimate),
  mxm = c(temp5$estimate,temp6$estimate),
  mxf = c(temp7$estimate,temp8$estimate)

)
yaniv_w_yuval_socials

###find cor between yuval priority dimension and yanivs dominance and trustworthiness dimensions
temp1 <- cor.test(as.numeric( fXm_dim), (yanivs_dom))
temp2 <- cor.test(as.numeric( fXm_dim), (yanivs_trust))
temp3 <- cor.test(as.numeric( fXf_dim), (yanivs_dom))
temp4 <- cor.test(as.numeric( fXf_dim), (yanivs_trust))
temp5 <- cor.test(as.numeric( mXm_dim), (yanivs_dom))
temp6 <- cor.test(as.numeric( mXm_dim), (yanivs_trust))
temp7 <- cor.test(as.numeric( mXf_dim), (yanivs_dom))
temp8 <- cor.test(as.numeric( mXf_dim), (yanivs_trust))

yuval_w_yaniv_socials <- data.table(
  'social Dimension' = c('dominance', 'trust'),
  fxm = c(temp1$estimate,temp2$estimate),
  fxf = c(temp3$estimate,temp4$estimate),
  mxm = c(temp5$estimate,temp6$estimate),
  mxf = c(temp7$estimate,temp8$estimate)
  
)
yuval_w_yaniv_socials

###find cor between yuval priority dimension and yanivs raw dominance and trustworthiness dimensions
temp1 <- cor.test(as.numeric( fXm_dim), (yanivs_raw_dom))
temp2 <- cor.test(as.numeric( fXm_dim), (yanivs_raw_trust))
temp3 <- cor.test(as.numeric( fXf_dim), (yanivs_raw_dom))
temp4 <- cor.test(as.numeric( fXf_dim), (yanivs_raw_trust))
temp5 <- cor.test(as.numeric( mXm_dim), (yanivs_raw_dom))
temp6 <- cor.test(as.numeric( mXm_dim), (yanivs_raw_trust))
temp7 <- cor.test(as.numeric( mXf_dim), (yanivs_raw_dom))
temp8 <- cor.test(as.numeric( mXf_dim), (yanivs_raw_trust))

yuval_w_yaniv_raw_socials <- data.table(
  'social Dimension' = c('dominance', 'trust'),
  fxm = c(temp1$estimate,temp2$estimate),
  fxf = c(temp3$estimate,temp4$estimate),
  mxm = c(temp5$estimate,temp6$estimate),
  mxf = c(temp7$estimate,temp8$estimate)
  
)
yuval_w_yaniv_raw_socials

### Load required libraries
#library(reshape)
library(psych)
#library(plyr)
#library(ggplot2)
#library(ggm)

### Create correlation table
ct.priority_dims_merged <- corr.test(priority_dims_merged)

### Plot priority heatmap
# Convert to long format
heatmap <- data.frame(ct.priority_dims_merged$r)
heatmap$name <- rownames(heatmap)
heatmap <- melt(heatmap, id.vars = 'name')
heatmap$name <- factor(heatmap$name, levels = rev(c('fXf', 'fXm','mXf',
                                                    'mXm', 'bothXm', 'yaniv')))

# Round r values to 2 digits
heatmap$label <- sprintf("%0.2f", round(heatmap$value,2))

# Plot
(p <- ggplot(heatmap, aes(x=variable, y=name))) +
  geom_tile(aes(fill=-value)) + geom_text(aes(label = label)) +
  scale_x_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv'),
                   expand = c(0,0), position = "top") +
  scale_y_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv'),
                   expand = c(0,0)) + theme(axis.ticks = element_blank(), 
                                            axis.text	= element_text(size=12),
                                            axis.text.x = element_text(angle = 45, hjust = 0)) +
  scale_fill_distiller("",palette = "RdYlBu", limits = c(-1,1)) +
  ggtitle("priority dimensions correlations")




### Create correlation table
ct.doms_merged <- corr.test(doms_merged)

### Plot dominance heatmap
# Convert to long format
heatmap <- data.frame(ct.doms_merged$r)
heatmap$name <- rownames(heatmap)
heatmap <- melt(heatmap, id.vars = 'name')
heatmap$name <- factor(heatmap$name, levels = rev(c('fXf', 'fXm','mXf',
                                                    'mXm', 'bothXm', 'yaniv', 'yaniv raw')))

# Round r values to 2 digits
heatmap$label <- sprintf("%0.2f", round(heatmap$value,2))

# Plot
(p <- ggplot(heatmap, aes(x=variable, y=name))) +
  geom_tile(aes(fill=-value)) + geom_text(aes(label = label)) +
  scale_x_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv',
                                'yaniv raw' = 'Yaniv raw'),
                   expand = c(0,0), position = "top") +
  scale_y_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv',
                                'yaniv raw' = 'Yaniv raw'),
                   expand = c(0,0)) + theme(axis.ticks = element_blank(), 
                                            axis.text	= element_text(size=12),
                                            axis.text.x = element_text(angle = 45, hjust = 0)) +
  scale_fill_distiller("",palette = "RdYlBu", limits = c(-1,1), direction = -1) +
  ggtitle("power/dominance dimensions correlations")


### Create correlation table
ct.trusts_merged <- corr.test(trusts_merged)

### Plot trust heatmap
# Convert to long format
heatmap <- data.frame(ct.trusts_merged$r)
heatmap$name <- rownames(heatmap)
heatmap <- melt(heatmap, id.vars = 'name')
heatmap$name <- factor(heatmap$name, levels = rev(c('fXf', 'fXm','mXf',
                                                    'mXm', 'bothXm', 'yaniv', 'yaniv raw')))

# Round r values to 2 digits
heatmap$label <- sprintf("%0.2f", round(heatmap$value,2))

# Plot
(p <- ggplot(heatmap, aes(x=variable, y=name))) +
  geom_tile(aes(fill=-value)) + geom_text(aes(label = label)) +
  scale_x_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv',
                                'yaniv raw' = 'Yaniv raw'),
                   expand = c(0,0), position = "top") +
  scale_y_discrete('', labels=c('fXf' = 'fXf',
                                'fXm' = 'fXm',
                                'mXf' = 'mXf',
                                'mXm' = 'mXm',
                                'bothXm' = 'bothXm',
                                'yaniv' = 'Yaniv',
                                'yaniv raw' = 'Yaniv raw'),
                   expand = c(0,0)) + theme(axis.ticks = element_blank(), 
                                            axis.text	= element_text(size=12),
                                            axis.text.x = element_text(angle = 45, hjust = 0)) +
  scale_fill_distiller("",palette = "RdYlBu", limits = c(-1,1), direction = -1) +
  ggtitle("valence/trustworthiness dimensions correlations")




#
cor.test(as.numeric( dom_fXm), (yanivs_priority))
cor.test(as.numeric( trust_fXm), (yanivs_priority))
cor.test(as.numeric( dom_fXf), (fXf_dim))
cor.test(as.numeric( trust_fXf), (fXf_dim))
cor.test(as.numeric( dom_mXm), (mXm_dim))
cor.test(as.numeric( trust_mXm), (mXm_dim))
cor.test(as.numeric( dom_mXf), (mXf_dim))
cor.test(as.numeric( trust_mXf), (mXf_dim))
