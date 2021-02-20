# Week 7 Individual Milestone
library(DAAG)
library(latticeExtra)
library(MASS)
library(car)
library(tidyr)
library(dplyr)

#Changes DataFrame into simpler variable name
fullData <- CompleteNBA_GroupProject_V6
fullData_Complete <- fullData %>% drop_na() 

# Narrowing the dataset to my individual position group, Small Forwards
SF <- filter(fullData, SF == 1)
SF_complete <- SF %>% drop_na()
# Creating a second version, where I will scale variables to by minutes
byMin_SF <- filter(fullData, SF == 1)

# Splitting Dataframe in Variables that capture per game rates and season totals
# The goal here is to select which type of stat we want to use for the model based on correlations/multi-collinearity
rateVars_SF <- select(SF, Player, Year, PTS, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG., G, GS, MP)
totalVars_SF <- select(SF, Player, Year, PTS, FT, FTA, FGA, X3PA, X2PA, ORB, DRB, TRB, AST, STL, BLK, TOV, PFouls, G, GS, MP)

# removing identifiers to create correlation table for variables
rateVars_SF_num <- select(rateVars_SF, PTS, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG., G, GS, MP)
totalVars_SF_num <- select(totalVars_SF, PTS, FT, FTA, FGA, X3PA, X2PA, ORB, DRB, TRB, AST, STL, BLK, TOV, PFouls, G, GS, MP)
# Due to concerns over multi-collinearity, I'm starting with exploring which variables exhibit multi-collinearity
cor(rateVars_SF_num, use = 'complete.obs')
cor(totalVars_SF_num, use = 'complete.obs')

# Initial Models based on earlier variable exploration
# These initial 3 models are evaluating which of G/GS/MP should be dropped due to Multi-collinearity
modelG <- lm(PTS ~ FTA + X3PA + G, data = SF)
summary(modelG)
modelG_MSE <- mean(modelG$residuals^2)
  
modelGS <- lm(PTS ~ FTA + X3PA + GS, data = SF)
summary(modelGS)
modelGS_MSE <- mean(modelGS$residuals^2)

modelMP <- lm(PTS ~ FTA + X3PA + MP, data = SF)
summary(modelMP)
modelMP_MSE <- mean(modelMP$residuals^2)
vif(modelMP)

# Below Model is a bad step from modelMP, leaving code here as a reminder
#model_logMP <- lm(PTS ~ FTA + X3PA + log(MP+1), data = SF)
#summary(model_logMP)
#model_logMP_MSE <- mean(model_logMP$residuals^2)

testModel <- lm(PTS ~ G, data = SF)
summary(testModel)
testModel_MSE <- mean(testModel$residuals^2)
vif(testModel)

# Backward Elminiation
numVars_SF <- dplyr::select(SF, PTS, FT, FTA, FGA, X3PA, X2PA, ORB, DRB, TRB, AST, STL, BLK, TOV, PFouls, G, GS, MP, Age, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG., height_cm, weight_kg, draft_round, draft_pick, true_draft)
numVars_SF <- numVars_SF %>% drop_na()
model_full <- lm(PTS ~ ., data = numVars_SF)
summary(model_full)

# help(stepAIC)
backwardStep <- stepAIC(model_full, direction = 'backward')
summary(backwardStep)
backwardStep$anova
backwardStep_MSE <- mean(backwardStep$residuals^2)
vif(backwardStep)

# Forward Selection
model_empty <- lm(PTS ~ 1, data = numVars_SF)
forwardStep <- stepAIC(model_empty, direction = 'forward', scope = list(upper=model_full, lower=model_empty))
summary(forwardStep)
forwardStep$anova
forwardStep_MSE <- mean(forwardStep$residuals^2)
vif(forwardStep)


# 10-Fold Cross Validation
# First I created a dataframe specific to the model I want to validate
# This allows me to drop observations that are missing values for the relevant variables only
SF_CFV <- dplyr::select(SF, PTS, FTA, X3PA, MP, TRB., TOV.) %>% drop_na() 
validatedModel <- cv.lm(data = SF_CFV, form.lm = formula(PTS ~ TOV. + FTA + X3PA + MP + TRB.), plotit = 'Observed', m = 10)
summary(validatedModel)
validatedModel_avgMSE <- 7871


#----------------------------
# Below is the Final Model I will be presenting for this week.
# But I am still considering whether or not to keep or drop MP & TRB
# MP keeps residuals down. TRB doesn't do much
# Both TOV. and TRB. could be replaced by log transformations of the total variables, but currently favor rates version for understability reasons
# Additionally swapping Free Throws made could be swapped with FTA and only decreases A R^2 by ~3%
week7_Model <- lm(PTS ~ TOV. + FTA + X3PA + MP, data = SF)
summary(week7_Model)
week7_Model_MSE <- mean(week7_Model$residuals^2)
vif(week7_Model)

median(SF$PTS)

