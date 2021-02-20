# DSC 423, Devin Carroll
# If you a fellow DePaul student stop reading this script.
# Assignment 3, Pisa2009 predicting readingScore
library(latticeExtra)
library(car)
library(DAAG)
library(MASS)
library(ggplot2)
library(dplyr)
library(corrplot)

fullData <- Pisa2009 # changing variable name
fullData$X <- NULL   # dropping old index variable

# Variable Exploration/Analysis
# Histograms of Each Variable, Variables are separated into rough Categories to help conceptualize

# Dependent/Response Variable
hist(fullData$readingScore) 
# normally distributed, mean = 518.5, median 520.2, some outliers in both directions but more low 

#Variables Related to the Student
hist(fullData$male)            # binary variable, 1=male, ~50-50 split
hist(fullData$preschool)       # binary variable, 1 = attended pre-school, ~2/3 have attended pre-school
hist(fullData$expectBachelors) # binary variable, 1 = expects to get bachelors, ~80% expect to get bachelors
hist(fullData$grade)           # ~ normal distribution but range is only 8-12
barplot(summary(fullData$raceeth))
# White = ~61%, Hispanic = ~20%, Black = ~9%, Asian = ~4%, Multi-Racial = ~3.4%, Native >1%

# Parent/Family Related Variables
hist(fullData$motherHS)        # binary variable, 1 = mom completed HS, +80% moms completed HS
hist(fullData$motherBachelors) # binary variable, 1 = mom has BS, ~1/3 moms have BS
hist(fullData$motherWork)      # binary variable, 1 = mom works, ~70% moms work
hist(fullData$motherBornUS)    # binary variable, 1 = mom born in US, ~80% moms born in US
hist(fullData$fatherHS)        # binary variable, 1 = dad completed HS, ~85% dads completed HS
hist(fullData$fatherBachelors) # binary variable, 1 = dad has BS, ~1/3 dads have BS
hist(fullData$fatherWork)      # binary variable, 1 = dad works, ~85% dads work
hist(fullData$fatherBornUS)    # binary variable, 1 = dad born in US, ~80% dads born in US
hist(fullData$selfBornUS)      # binary variable, 1 = born in US, +90% born in US
hist(fullData$englishAtHome)   # binary variable, 1 = speaks English at home, ~85% speak English at home

# Variables Related to Student's Outside of School Educational Access
hist(fullData$computerForSchoolwork) # binary variable, 1 = access to computer, +85% have access to computer 
hist(fullData$read30MinsADay)        # binary variable, 1 = reads 30 minutes/day at home, ~1/3 read 30 minutes/day at home

# Variables Related to Student's School
hist(fullData$schoolSize)            # number of students at school, slightly left skew, Median = 1233, Mean = 1373, ~5 outliers
hist(fullData$minutesPerWeekEnglish) # Right Skewed, Median=250, Mean=269, very narrow IQR and lots of outliers in both directions
hist(fullData$studentsInEnglish)     # ~ Normally Distributed, some outliers in both directions, Median = 25, Mean = 24.59
hist(fullData$schoolHasLibrary)      # binary variable, 1 = school has library, +90% have library
hist(fullData$publicSchool)          # binary variable, 1 = attends public school, +90% attend public school
hist(fullData$urban)                 # binary variable, 1 = attends urban school, ~1/3 attend urban schools, ~2/3 attend non-urban schools

# Variable Summaries
summary(fullData$raceeth)
summary(fullData$minutesPerWeekEnglish)
summary(fullData$studentsInEnglish)
summary(fullData$schoolSize)
summary(fullData$readingScore)
summary(fullData$grade)
# Box Plot
boxplot(fullData$minutesPerWeekEnglish)
boxplot(fullData$studentsInEnglish)
boxplot(fullData$schoolSize) # R only shows 5 outlier values but IQR method I was taught shows a lot more
boxplot(fullData$readingScore)

# Scatter Plots
#Variables Related to the Student
plot(fullData$grade, fullData$readingScore) # range seems pretty similar between grade levels
plot(fullData$male, fullData$readingScore) # women have a slightly higher peak and floor
plot(fullData$preschool, fullData$readingScore) # little difference between pre-schooled vs non
plot(fullData$expectBachelors, fullData$readingScore) # slightly higher peak among students who expect to get BS
# TBD Race

# Parent/Family Related Variables
plot(fullData$motherHS, fullData$readingScore) # slightly larger range (higher peak/lower floor) for students with HS grad Moms
plot(fullData$motherBachelors, fullData$readingScore) # no real difference in range between students with BS Moms
plot(fullData$motherWork, fullData$readingScore) # no real difference between students with working vs non-working mothers
plot(fullData$motherBornUS, fullData$readingScore) # no real difference
plot(fullData$fatherHS, fullData$readingScore) # slightly better outcomes for students with HS grad fathers, but mostly the same
plot(fullData$fatherBachelors, fullData$readingScore) # no real difference
plot(fullData$fatherWork, fullData$readingScore) # no real difference
plot(fullData$fatherBornUS, fullData$readingScore) # no real difference
plot(fullData$selfBornUS, fullData$readingScore) # non-US born students have a slightly smaller range

# Variables Related to Student's Outside of School Educational Access
plot(fullData$computerForSchoolwork, fullData$readingScore) # slightly larger range for students with computer access
plot(fullData$read30MinsADay, fullData$readingScore) # floor is lower for students who don't read 30 mins/day

# Variables Related to Student's School
plot(fullData$minutesPerWeekEnglish, fullData$readingScore) # not a clear relationship between time in English class and score
plot(fullData$studentsInEnglish, fullData$readingScore) # no clear relationship between class size and score
plot(fullData$schoolHasLibrary, fullData$readingScore) # slightly larger range of outcomes for students at schools with libraries
plot(fullData$publicSchool, fullData$readingScore) 
# floor is definitely higher for private school students, though the peak is also slightly lower
plot(fullData$urban, fullData$readingScore) # no real difference
plot(fullData$schoolSize, fullData$readingScore) # no clear relationship

# Creating Dummy Variables for Race
# this isn't really needed since LM will do it for you.
fullData$race_Native <- ifelse(fullData$raceeth == 'American Indian/Alaska Native', 1, 0)
fullData$race_Asian <- ifelse(fullData$raceeth == 'Asian', 1, 0)
fullData$race_AA <- ifelse(fullData$raceeth == 'Black', 1, 0)
fullData$race_Hispanic <- ifelse(fullData$raceeth == 'Hispanic', 1, 0)
fullData$race_Plus1 <- ifelse(fullData$raceeth == 'More than one race', 1, 0)
fullData$race_Island <- ifelse(fullData$raceeth == 'Native Hawaiian/Other Pacific Islander', 1, 0)
# White/Caucasian is default because it is the most numerous

# Correlation Table
tempData = fullData # create a temp df
tempData$raceeth <- NULL # drop race variable
corTable <- cor(tempData) # create correlation table
# no variables with clearly strong correlation
corrplot(corTable, method ='color', addCoef.col = 'black',
         type='upper',tl.col = 'black')
readingScore_Cor <- corTable[,23]
readingScore_Cor

racePivotTalble <- fullData %>% group_by(raceeth) %>% summarize(mean_readingScore = mean(readingScore, na.rm = T), 
              sd_readingScore = sd(readingScore, na.rm = T), 
              median_readingScore = median(readingScore, na.rm = T))
racePivotTalble
overallMean = mean(fullData$readingScore)
overallMedian = median(fullData$readingScore)
overallSD = sd(fullData$readingScore)


#---------------------------- OLD CODE Leaving here for prosperity
# This shows LM will automatically create dummy variables for Factor variables
model1 <- lm(readingScore ~ ., data = fullData)
summary(model1)

model2 <- lm(readingScore ~ ., data = Pisa2009)
summary(model2)
#----------------------------------

# Exploring Potential Transformations: minutesPerWeekEnglish, schoolSize, studentsInEnglish, grade
cor(fullData$readingScore, log(fullData$minutesPerWeekEnglish+1))
cor(fullData$readingScore, log(fullData$schoolSize+1))
cor(fullData$readingScore, log(fullData$studentsInEnglish+1))
cor(fullData$readingScore, log(fullData$grade+1))


# Checking correlation when a natural log is applied
fullData$readingScore_Ln <- log(fullData$readingScore)
tempData = fullData # create a temp df
tempData$raceeth <- NULL # drop race variable
corTable2 <- cor(tempData)
readingScore_Ln_Cor <- corTable[,24]
readingScore_Ln_Cor

model3 <- lm(readingScore_Ln ~ schoolSize, data = fullData)
summary(model3)

model4 <- lm(readingScore ~ schoolSize, data = fullData)
summary(model4)

# Attempting to Z-Score Normalize Variables
normalize <- function(x){
  return ((x-mean(x, na.rm = T))/sd(x, na.rm = T))
}
normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

testData = fullData
testData$readingScore_Norm = testData$readingScore
testData[25] <- apply(testData[25], 2, normFunc) # maybe not work if other readingScore columns are removed
testData[19] <- apply(testData[19], 2, normFunc)
testData[22] <- apply(testData[22], 2, normFunc)

testData$readingScore_Ln = NULL
testData$readingScore = NULL
testData$raceeth = NULL
cor(testData)

modelNorm <- lm(readingScore_Norm ~ minutesPerWeekEnglish, data = testData)
summary(modelNorm)

# Stepwise Model Building bc I have no idea how to proceed
# Backward Elminiation
model_full <- lm(readingScore_Norm ~ ., data = testData)
summary(model_full)

# help(stepAIC)
backwardStep <- stepAIC(model_full, direction = 'backward')
summary(backwardStep)
backwardStep$anova
backwardStep_MSE <- mean(backwardStep$residuals^2)
vif(backwardStep)

# Forward Selection
model_empty <- lm(readingScore_Norm ~ 1, data = testData)
forwardStep <- stepAIC(model_empty, direction = 'forward', scope = list(upper=model_full, lower=model_empty))
summary(forwardStep)
forwardStep$anova
forwardStep_MSE <- mean(forwardStep$residuals^2)
vif(forwardStep)


# Looking for Interaction Terms
testModel <- lm(readingScore ~ motherWork + fatherWork + motherWork*fatherWork, data = Pisa2009)
testModel <- lm(readingScore_Norm ~ minutesPerWeekEnglish + studentsInEnglish + minutesPerWeekEnglish*studentsInEnglish, data = testData)
summary(testModel)

summary(fullData$minutesPerWeekEnglish)
noOut_Pisa <- filter(Pisa2009, minutesPerWeekEnglish <= 1200)

fullData
testModel2 <- lm(readingScore ~ expectBachelors + log(minutesPerWeekEnglish+1) + read30MinsADay + motherBachelors + fatherBachelors
                 + grade + (motherBachelors*fatherBachelors) + englishAtHome + publicSchool + urban, data = testData)
summary(testModel2)


