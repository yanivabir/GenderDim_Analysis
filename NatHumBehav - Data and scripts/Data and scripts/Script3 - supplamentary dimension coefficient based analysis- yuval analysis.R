# Analyse correlations between diemsnion coefficients

### Load required libraries
library(reshape)
library(psych)
library(plyr)
library(ggplot2)
library(ggm)

### Set working directory
# Change this to the directory path you're working from
setwd('/Users/yanivabir/Google Drive/Lab/Todorov/Paper/Data and scripts for submission')

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
extractDimension <- function(x, faces, result = "dimension") {
  
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

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp1,aggregate(exp1[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs <- data.frame(extractDimension(tmp$ZRT, faces = faces))
colnames(coeffs) <- 'normalExp1'

### Load Experiment 2 data
excludeSubjects <- c(16,26,37,56)
exp2 <- read.csv('./Data/exp2Data.csv')
exp2 <- cleanData(exp2, excludeSubjects = excludeSubjects)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp2,aggregate(exp2[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$normalExp2 <- extractDimension(tmp$ZRT, faces = faces)

### Combine Experiments 1 and 2 to create the priority-dimension
# Make sure subject numbers are unique
exp1$Subject <- exp1$Subject + 100
exp2$Subject <- exp2$Subject + 200
# Combine
combined <- rbind(exp1, exp2)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(combined,aggregate(combined[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$priority <- extractDimension(tmp$ZRT, faces = faces) # BTs predicted from the priority-dimension

### Load Experiment 4 data
exp4 <- read.csv('./Data/exp4Data.csv')
excludeSubjects <- list(5, 19, 27, 36)
exp4 <- cleanData(exp4, excludeSubjects = excludeSubjects)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp4,aggregate(exp4[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$scrambled <- extractDimension(tmp$ZRT, faces = faces)

### Load Experiment 5 data
exp5 <- read.csv('./Data/exp5Data.csv')
excludeSubjects <- list(70, 12, 17, 24, 36, 39, 54, 62)
exp5 <- cleanData(exp5, excludeSubjects = excludeSubjects)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp5,aggregate(exp5[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$flipped <- extractDimension(tmp$ZRT, faces = faces)

### Open experiment 6 data - conscious condition
exp6consc <- read.csv('./Data/Exp6ConscData.csv')
excludeSubjects = c(3,4,6,8,9,10,13,14,16,18,19,22,25,28,33,36,38,44,45,50,58)
exp6consc <- cleanData(exp6consc, excludeSubjects = excludeSubjects)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp6consc,aggregate(exp6consc[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$consc <- extractDimension(tmp$ZRT, faces = faces)

### Open experiment 6 data - non - conscious condition
exp6noncon <- read.csv('./Data/Exp6NonConData.csv')
exp6noncon <- cleanData(exp6noncon, excludeSubjects = excludeSubjects)

## Add coefficients to summary dataframe
# Average over stimuli repetitions
tmp <- with(exp6noncon,aggregate(exp6noncon[c("ZRT")], by=list(Subject,Stimulus),FUN=mean))
# Average over subjects
tmp <- with(tmp,aggregate(tmp[c("ZRT")], by=list(Group.2),FUN=mean))
coeffs$noncon6 <- extractDimension(tmp$ZRT, faces = faces)

## Load trait PCs
trait <- read.csv("300_OriginalFaces_Trait&Gender_Data.csv")
colnames(trait)[5:6] <- c("Valence", "Power")
coeffs$Valence <- extractDimension(trait$Valence, faces = faces)
coeffs$Power <- extractDimension(trait$Power, faces = faces)
# Rotate power dimension, following Oosterhof and Todorov (2008)
#coeffs$rotPower <- resid(lm(coeffs$Power ~ coeffs$Valence))#######  not necassery for now

### Create correlation table
ct.coeff <- corr.test(coeffs[,c(3,7,6,5,4,8,9)])

### Plot heatmap
# Convert to long format
heatmap <- data.frame(ct.coeff$r)
heatmap$name <- rownames(heatmap)
heatmap <- melt(heatmap, id.vars = 'name')
heatmap$name <- factor(heatmap$name, levels = rev(c('priority', 'noncon6','consc',
                                                'flipped', 'scrambled', 'Valence',
                                                'Power')))

# Round r values to 2 digits
heatmap$label <- sprintf("%0.2f", round(heatmap$value,2))

# Plot
(p <- ggplot(heatmap, aes(x=variable, y=name))) +
  geom_tile(aes(fill=-value)) + geom_text(aes(label = label)) +
  scale_x_discrete('', labels=c('priority' = 'Priority \ndimension',
                                'noncon6' = 'Experiment 6 - \nnon-conscious',
                                'consc' = 'Conscious \ncontrol',
                                'flipped' = 'Flipped \nfaces',
                                'scrambled' = 'Scrambled \nfaces',
                                'Valence' = 'Valence',
                                'Power' = 'Power /\ndominance'),
                   expand = c(0,0), position = "top") +
  scale_y_discrete('', labels=c('priority' = 'Priority \ndimension',
                                'noncon6' = 'Experiment 6 - \nnon-conscious',
                                'consc' = 'Conscious \ncontrol',
                                'flipped' = 'Flipped faces',
                                'scrambled' = 'Scrambled \nfaces',
                                'Valence' = 'Valence',
                                'Power' = 'Power /\ndominance'),
                   expand = c(0,0)) + theme(axis.ticks = element_blank(), 
                                            axis.text	= element_text(size=12),
                                            axis.text.x = element_text(angle = 45, hjust = 0)) +
  scale_fill_distiller("",palette = "RdYlBu", limits = c(-1,1))

exp_dims_data <- coeffs[8:9]
write.csv(exp_dims_data, "yanivs_raw_social_dims.csv")
