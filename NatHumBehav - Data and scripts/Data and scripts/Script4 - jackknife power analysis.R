# Jackknife power analysis for finding a consistent dimension, based on Experiment 1 data
library(bootstrap)
library(pwr)
library(plyr)
library(psych)

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

# Load Experimet 1 data
### Load Experiment 1 data
exp1 <- read.csv('./Data/exp1Data.csv')
excludeSubjects = c(26) # Subjects 7 and 8 were excluded at the single file level, before anaylysis procedure was set
exp1 <- cleanData(exp1, excludeSubjects = excludeSubjects)

### Load parameter matrix for 300 randomly generated faces
faces <- read.csv('oosterhof_todorov_300_faces_component_values.csv')
faces$Stimulus <- levels(exp1$Stimulus)

### Average over repetitions
exp1 <- ddply(exp1, .(Subject,Stimulus), summarise, ZRT = mean(ZRT))

### Define jackknife function
jackCor <- function(x, xdat, faces) {

  # Select data for the subject, and all the rest
  subjects <- unique(xdat$Subject)
  s <- xdat[!(xdat$Subject %in% x), ]
  rest <- xdat[xdat$Subject %in% x, ]
  rest <- ddply(rest, .(Stimulus), summarise, ZRT = mean(ZRT))
  
  # Prepare faces - remove faces for which the subject has missing values
  subjfaces <- subset(faces, faces$Stimulus %in% s$Stimulus)
  subjfaces <- data.matrix(subjfaces[,1:50])
  
  # Group faces - no missing values assumed
  faces <- data.matrix(faces[,1:50])
  
  # De-mean
  s <- s$ZRT - mean(s$ZRT)
  rest <- rest$ZRT - mean(rest$ZRT)
  
  # Dot product (weighted average)
  subjDim <- s %*% subjfaces
  restDim <- rest %*% faces
  
  # Normalize
  subjDim <- t(subjDim / sqrt(sum(subjDim^2)))
  restDim <- t(restDim / sqrt(sum(restDim^2)))
  
  return(cor(subjDim, restDim))
}

## Run jackknife procedure
cors <- jackknife(unique(exp1$Subject), function(x) jackCor(x, exp1, faces))

# Plot result in histogram, after Fisher's transformation
hist(fisherz(cors$jack.values))

# Compute Cohen's d
d <- t.test(fisherz(cors$jack.values))
d <- d$statistic / sqrt(d$parameter)

# Power analysis
pwr.t.test(power = .95, d = d, sig.level = .05)
