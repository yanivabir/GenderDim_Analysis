## Analyse Experiment 3 - dimension validation

library(reshape)
library(plyr)
library(ggplot2)
library(ez)
library(lme4)
library(psych)


### Set working directory
# Change this to the directory path you're working from
setwd('/Users/yanivabir/Downloads/Data and scripts')

### Load data
exp3 <- read.csv('./Data/exp3Data.csv')

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


### Clean data
excludeSubjects = list(9,13,16,18,22,24,27,34,37,44,48,47)
exp3 <- cleanData(exp3, excludeSubjects = excludeSubjects)

### Trasform dimension level to z score units
exp3$Level <- exp3$Level/2-5.5

# Repeated measures planned linear contrast
cont <- with(exp3,ezANOVA(exp3,
                                dv = ZRT,
                                wid = factor(Subject),
                                within = Level,
                                type = 3,
                                detailed = TRUE,
                                return_aov = TRUE))
print(cont)

### Plot the data
# Average over repetitions
exp3.agg <- cast(exp3, Subject + Level ~ ., mean, value = "ZRT")
colnames(exp3.agg) <- c("Subject", "Level", "ZRT")

# Average over subjects
exp3.agg <- ddply(exp3.agg, "Level", summarize, mean = mean(ZRT), sd = sd(ZRT) ,se = sd(ZRT) / sqrt(length(ZRT)))

# Get repetaed-measures coefficient for trendline
exp3.coef <- coef(cont$aov)

# Create trend line data
exp3.agg$predicted <- exp3.coef$`(Intercept)` + exp3.agg$Level * 
  exp3.coef$`Subject:Level`

# Plot
p <- ggplot(exp3.agg)
p + geom_smooth(aes(x=Level, y=mean), method = "lm", se = F, colour="#000000") +
  geom_pointrange(aes(x=Level, y=mean, ymin = mean-se, ymax = mean+se), colour="#808080") +
  geom_point(aes(x=Level, y=mean), colour ="#808080", shape=19) +
  labs(x = "Priority-dimension (SD)", y = "Std. breaking-time") +
  theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(),
        text = element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Mixed model analysis
# Log trasnform. Use ms as units
exp3$RT.log <- log(exp3$RT*1000)

# Fit linear model with by subject random slope and intercept
mod.log <- lmer(RT.log ~ Level + (1 + Level | Subject), exp3)
summary(mod.log)

# Fit lesser model (no level factor)
mod.logless <- lmer(RT.log ~ (1 + Level | Subject), exp3)
# Compare models with likelihood ratio test
anova(mod.log, mod.logless)


### Reliability
## Test-retest reliability
exp3$Half <- ifelse(exp3$Block < 29/2, 1, 2)
rVars <- c("Stimulus","Half","ZRT")
mdata <- melt(exp3[rVars],id=c("Stimulus","Half"))
mdata <- cast(mdata,Stimulus~Half,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(corr.test(mdata), short = FALSE)

## inter participant agreement
rVars <- c("Stimulus","Subject","ZRT")
mdata <- melt(exp3[rVars],id=c("Stimulus","Subject"))
mdata <- cast(mdata,Stimulus~Subject,mean)
rownames(mdata) <- mdata$Stimulus
mdata$Stimulus <- NULL

print(ICC(mdata, missing = FALSE))

