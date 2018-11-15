# This is the main analysis script, for experiments 1,2,4-6. 
# The script loads the raw data files, merges them, prepares the data and runs the
# anlyses.

### Load required libraries
library(reshape)
library(psych)
library(plyr)
library(ggplot2)
library(ggm)

### Set working directory
# Change this to the directory path you're working from
setwd('PATH/TO/Data and scripts')

### Define data preparation procedure as a function
cleanData <- function(x, excludeSubjects = NULL) {
  # Code a missing response as wrong response
  x$Acc[x$Acc != 1] <- 0
  
  # Exclude subjects by their debrief answers
  x <- subset(x, !(Subject %in% excludeSubjects))
  
  # Calculate mean accuracy per subject
  acc <- aggregate(x[c("Subject","Acc")], by = list(x$Subject), FUN = mean)
  print(nrow(acc)) # Print number of subjects prior to exclusion
  acc <- subset(acc, Acc >= .9) # select by acuuracy criterion
  print(nrow(acc)) # Print number of subjects after exclusion
  x <- subset(x, Subject %in% acc$Subject)
  
  # Exclude trials by accuracy
  pre <- nrow(x)
  x <- subset(x, Acc == 1)
  print('Excluded by accuracy')
  print(c(pre - nrow(x), (pre - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Exclude trials by minimum response time criterion
  tmp <- nrow(x)
  x <- subset(x, RT > .2)
  print("RT > .2")
  print(c(tmp - nrow(x), (tmp - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Standardize per subject
  x$ZRT <- ave(x$RT, x$Subject, FUN = scale)
  
  # Exclude by 3 SD exclusion criterion
  tmp <- nrow(x)
  x <- subset(x, abs(ZRT) < 3)
  x$ZRT <- NULL
  print("ZRT < 3")
  print(c(tmp - nrow(x), (tmp - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Standardize
  x$ZRT <- ave(x$RT, x$Subject, FUN = scale)
  
  return(x)
}

### Define dimension extraction procedure as a function
extractDimension <- function(x, faces, result = "scores") {
  
  paramRange <- 1:50 # Extract using symmetrical face parameters
  faces <- data.matrix(faces[,paramRange])
  
  # De-mean
  x <- x - mean(x)
  
  # Dot product (weighted average)
  Dim <- x %*% faces
  
  # Normalize
  Dim <- t(Dim / sqrt(sum(Dim^2)))
  
  # Return diemsnion, or dimension scores (computed as projection of each face on dimension)
  return(switch(result, scores = drop(faces %*% Dim), dimension = Dim))
}

### Load parameter matrix for 300 randomly generated faces
faces <- read.csv('oosterhof_todorov_300_faces_component_values.csv')

### Load Experiment 1 data
exp1 <- read.csv('./Data/exp1Data.csv')
excludeSubjects = c(26) # Subjects 7 and 8 were excluded at the single file level, before anaylysis procedure was set
exp1 <- cleanData(exp1, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp1,aggregate(exp1[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values <- data.frame(tmp$ZRT)
colnames(values) <- 'normalExp1'
rownames(values) <- tmp$Group.1
values$normalExp1Scores <- extractDimension(values$normalExp1, faces = faces)

### Load Experiment 2 data
excludeSubjects <- c(16,26,37,56)
exp2 <- read.csv('./Data/exp2Data.csv')
exp2 <- cleanData(exp2, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp2,aggregate(exp2[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$normalExp2 <- tmp$ZRT
values$normalExp2Scores <- extractDimension(values$normalExp2, faces = faces)

### Combine Experiments 1 and 2 to create the priority-dimension
# Make sure subject numbers are unique
exp1$Subject <- exp1$Subject + 100
exp2$Subject <- exp2$Subject + 200
# Combine
combined <- rbind(exp1, exp2)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(combined,aggregate(combined[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$combined <- tmp$ZRT # Combined samples BTs
values$priority <- extractDimension(values$combined, faces = faces) # BTs predicted from the priority-dimension

### Load Experiment 4 data
exp4 <- read.csv('./Data/exp4Data.csv')
excludeSubjects <- list(5, 19, 27, 36)
exp4 <- cleanData(exp4, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp4,aggregate(exp4[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$scrambled <- tmp$ZRT
values$scrambledScores <- extractDimension(values$scrambled, faces = faces)


### Load Experiment 5 data
exp5 <- read.csv('./Data/exp5Data.csv')
excludeSubjects <- list(70, 12, 17, 24, 36, 39, 54, 62)
exp5 <- cleanData(exp5, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp5,aggregate(exp5[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$flipped <- tmp$ZRT
values$flippedScores <- extractDimension(values$flipped, faces = faces)

### Open experiment 6 data - conscious condition
exp6consc <- read.csv('./Data/Exp6ConscData.csv')
excludeSubjects = c(3,4,6,8,9,10,13,14,16,18,19,22,25,28,33,36,38,44,45,50,58)
exp6consc <- cleanData(exp6consc, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp6consc,aggregate(exp6consc[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$conscRT <- tmp$ZRT
values$conscScores <- extractDimension(values$conscRT, faces = faces)

### Open experiment 6 data - non - conscious condition
exp6noncon <- read.csv('./Data/Exp6NonConData.csv')
exp6noncon <- cleanData(exp6noncon, excludeSubjects = excludeSubjects)

## Add BTs and predicted BTs to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp6noncon,aggregate(exp6noncon[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$noncon6 <- tmp$ZRT
values$noncon6Scores <- extractDimension(values$noncon6, faces = faces)

### Load Oosterhof and Todorov trait summary PCs
trait <- read.csv("O&T300FacesTraitPCs.csv")
values$Valence <- trait$Valence
values$Power <- trait$Power

### Print correlation table
ct <- corr.test(values)

### Test difference between correlations
# Priority dimenion: difference b/w Valence and Power
dt <- r.test(n=300, 
             r12 = ct$r["Power","priority"], 
             r13 = ct$r["Valence","priority"], 
             r23 = ct$r["Power","Valence"])
print(dt)

# Priority dimension: BTs in experiment 6, conscious RTs in experiment 6
dt <- r.test(n=300, 
             r12 = ct$r["priority", "noncon6"], 
             r13 = ct$r["priority", "conscRT"], 
             r23 = ct$r["noncon6","conscRT"])
print(dt)

# Power with predicted BTs and RTs in experiment 6
dt <- r.test(n=300, 
             r12 = ct$r["Power", "noncon6Scores"], 
             r13 = ct$r["Power", "conscScores"], 
             r23 = ct$r["noncon6Scores","conscScores"])
print(dt)

# Valence with predicted BTs and RTs in experiment 6
dt <- r.test(n=300, 
             r12 = ct$r["Valence", "noncon6Scores"], 
             r13 = ct$r["Valence", "conscScores"], 
             r23 = ct$r["noncon6Scores","conscScores"])
print(dt)

### Partial correlation analyses
## Between Experiment 1 and 2 controlling for flipped, scrambled

# Exp 2 predicts 1 controlling for flipped
rnn.f <- pcor(c("normalExp1","normalExp2Scores","flippedScores"),var(values[c("normalExp1","normalExp2Scores","flippedScores")]))
print(rnn.f)
pcor.test(rnn.f,1,n=300)

# Exp 1 predicts 2 controlling for flipped
rnn.f <- pcor(c("normalExp1Scores","normalExp2","flippedScores"),var(values[c("normalExp1Scores","normalExp2","flippedScores")]))
print(rnn.f)
pcor.test(rnn.f,1,n=300)

# Exp 2 predicts 1 controlling for scrambled
rnn.s <- pcor(c("normalExp1","normalExp2Scores","scrambledScores"),var(values[c("normalExp1","normalExp2Scores","scrambledScores")]))
print(rnn.s)
pcor.test(rnn.s,1,n=300)

# Exp 1 predicts 2 controlling for scrambled
rnn.s <- pcor(c("normalExp1Scores","normalExp2","scrambledScores"),var(values[c("normalExp1Scores","normalExp2","scrambledScores")]))
print(rnn.s)
pcor.test(rnn.s,1,n=300)

# Exp 2 predicts 1 controlling for scrambled and flipped
rnn.sf <- pcor(c("normalExp1","normalExp2Scores","scrambledScores","flippedScores"),var(values[c("normalExp1","normalExp2Scores","scrambledScores","flippedScores")]))
print(rnn.sf)
pcor.test(rnn.sf,2,n=300)

# Exp 1 predicts 2 controlling for scrambled and flipped
rnn.sf <- pcor(c("normalExp1Scores","normalExp2","scrambledScores","flippedScores"),var(values[c("normalExp1Scores","normalExp2","scrambledScores","flippedScores")]))
print(rnn.sf)
pcor.test(rnn.sf,2,n=300)

# Prioirty dimension predicting Experiment 6 non conscious, controllin for conscios RT
rnn.c <- pcor(c("priority","noncon6","conscScores"),var(values[c("priority","noncon6","conscScores")]))
print(rnn.c)
pcor.test(rnn.c,1,n=300)

### Test-retest reliability
# Test retest for Experiment 1
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp1[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)

# Test retest for Experiment 2
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp2[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)

# Test retest for Experiment 4
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp4[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)

# Test retest for Experiment 5
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp5[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)

# Test retest for Experiment 1 and 2 combined
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(combined[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)


# Test retest for Experiment 6 - BTs
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp6noncon[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

nonconTrt <- corr.test(mdata)
print(nonconTrt, short = FALSE)

# Test retest for Experiment 6 - conscious control
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(exp6consc[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

conscTrt <- corr.test(mdata)
print(conscTrt, short = FALSE)

# Difference between test-retest reliabilty for BTs and coscious RTs in Experiment 6
# Valence with predicted BTs and RTs in experiment 6
dt <- r.test(n=300, 
             r12 = nonconTrt$r[2], 
             r34 = conscTrt$r[2])
print(dt)

### Compute inter-rater aggreement
# ICC for Experiment 1
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp1[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiment 2
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp2[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiments 1 and 2 combined
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(combined[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiment 4
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp4[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiment 5
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp5[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiment 6 - BTs
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp6noncon[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiment 6 - Conscoius RTs
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp6consc[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))


### Within subject comparison for Experiment 6
tmp1 <- cast(exp6consc[c("Subject","Stimulus","ZRT")], Subject + Stimulus ~ ., fun.aggregate = mean)
colnames(tmp1) <- c("Subject","Stimulus","conscRT")

tmp2 <- cast(exp6noncon[c("Subject","Stimulus","ZRT")], Subject + Stimulus ~ ., fun.aggregate = mean)
colnames(tmp2) <- c("Subject","Stimulus","BT")

withinSubj <- merge(tmp1,tmp2, by=c("Subject","Stimulus"))

rWithinSubj <- ddply(withinSubj,.(Subject),function (x) cor(x[c("conscRT","BT")])[2])
rHist <- hist(rWithinSubj$V1, 
              main = "Histogram of correlation between conscoius RT and BT over subjects",
              xlab = "Pearson's r", breaks = 10)

print(mean(rWithinSubj$V1))
print(sd(rWithinSubj$V1))
print(t.test(rWithinSubj$V1))

### Compare BTs and conscious RTs for Experiment 6
tmp1 <- cast(exp6consc[c("Subject","RT")], Subject ~ ., fun.aggregate = mean)
colnames(tmp1) <- c("Subject","conscRT")

tmp2 <- cast(exp6noncon[c("Subject","RT")], Subject ~ ., fun.aggregate = mean)
colnames(tmp2) <- c("Subject","BT")

compRT <- merge(tmp1, tmp2, by = "Subject")

print(t.test(compRT$conscRT,compRT$BT, paired = TRUE))
print(c(mean(compRT$conscRT), sd(compRT$conscRT)))
print(c(mean(compRT$BT), sd(compRT$BT)))

### Get mean and SD for BTs in Experiment 1
tmp1 <- cast(exp1[c("Subject","RT")], Subject ~ ., fun.aggregate = mean)
colnames(tmp1) <- c("Subject","RT")
print(c(mean(tmp1$RT), sd(tmp1$RT)))

### Plot histogram of BTs per participant for experiment 1 and 2
tmp1 <- cast(combined[c("Subject","RT")], Subject ~ ., fun.aggregate = mean)
colnames(tmp1) <- c("Subject","RT")
print(c(mean(tmp1$RT), sd(tmp1$RT)))

p <- ggplot(tmp1, aes(x=RT))
p + geom_histogram(binwidth = .15) +
  theme_bw() + xlab('Mean BT (s)') +
  ylab('Number of participants')+ 
  theme(panel.border = element_blank(), axis.line = element_line(),
                                           text = element_text(size=16))

### Plot histogram of standardised BT over faces
p <- ggplot(values, aes(x=combined))
p + geom_histogram() +
  theme_bw()+ xlab('Mean standardised BT (z score)') +
  ylab('Number of faces')+ 
  theme(panel.border = element_blank(), axis.line = element_line(),
                                 text = element_text(size=16))

### Hemifield comparison for priority-dimension
combined$Location <- factor(combined$Location)
VF.RT <- ddply(combined, .(Subject, Location), summarise, ZRT = mean(ZRT))
VF.RT <- cast(VF.RT, Subject ~ Location)
t.test(VF.RT$`9`, VF.RT$`10`, paired = T)

# Lower hemifield (location = 10)
hemi.lower <- subset(combined, Location == 10)
# Uppwer hemifield (location = 9)
hemi.upper <- subset(combined, Location == 9)

# Test retest for Experiment 1 and 2 combined - lower hemifield
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(hemi.lower[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

tRt.lower <- corr.test(mdata)
print(tRt.lower, short = FALSE)

# Test retest for Experiment 1 and 2 combined - upper hemifield
rVars <- c("Stimulus","Block","ZRT")
mdata <- melt(hemi.upper[rVars],id=c("Stimulus","Block"))
mdata <- cast(mdata,Stimulus~Block,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

tRt.upper <- corr.test(mdata)
print(tRt.upper, short = FALSE)


# Difference in test-retest reliability b/w lower and upper hemifield

dt <- r.test(n=300, 
             r12 = tRt.upper$r[2], 
             r34 = tRt.lower$r[2])
print(dt)

# ICC for Experiments 1 and 2 combined - lower hemifield
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(hemi.lower[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

# ICC for Experiments 1 and 2 combined - upper hemifield
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(hemi.upper[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

##* Split the priority-diemsnions by visual hemifield (lower/upper)
## Lower
# Average over stimuli repetitions
tmp <- with(hemi.lower,aggregate(hemi.lower[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$hemi.lower <- tmp$ZRT # Combined samples BTs
values$priority.lower <- extractDimension(values$hemi.lower, faces = faces) # BTs predicted from the priority-dimension

## Upper
# Average over stimuli repetitions
tmp <- with(hemi.upper,aggregate(hemi.upper[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
values$hemi.upper <- tmp$ZRT # Combined samples BTs
values$priority.upper <- extractDimension(values$hemi.upper, faces = faces) # BTs predicted from the priority-dimension

## Print correlation table
ct.lowup <- corr.test(values)

# Test for difference in correlation of BT with valence
dt <- r.test(n=300, 
             r12 = ct.lowup$r["Valence", "hemi.upper"], 
             r13 = ct.lowup$r["Valence", "hemi.lower"], 
             r23 = ct.lowup$r["hemi.upper","hemi.lower"])
print(dt)

# Test for difference in correlation of BT with power
dt <- r.test(n=300, 
             r12 = ct.lowup$r["Power", "hemi.upper"], 
             r13 = ct.lowup$r["Power", "hemi.lower"], 
             r23 = ct.lowup$r["hemi.upper","hemi.lower"])
print(dt)

# Test for difference in correlation of dimension scores with valence
dt <- r.test(n=300, 
             r12 = ct.lowup$r["Valence", "priority.upper"], 
             r13 = ct.lowup$r["Valence", "priority.lower"], 
             r23 = ct.lowup$r["priority.upper","priority.lower"])
print(dt)

# Test for difference in correlation of dimension scores with power
dt <- r.test(n=300, 
             r12 = ct.lowup$r["Power", "priority.upper"], 
             r13 = ct.lowup$r["Power", "priority.lower"], 
             r23 = ct.lowup$r["priority.upper","priority.lower"])
print(dt)

