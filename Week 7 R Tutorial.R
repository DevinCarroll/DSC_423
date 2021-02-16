# Week 7 R Tutorial
# Variable Tranformation/Engineering & Multicollinearity

# 7.4
# Density determined from underwater weighing
# Percent body fat from Siri's 1956 Equation
# Var 1 BodyFat
# Var 2 Weight in Lbs
# Var 3 Chest Circumference in cm
# Var 4 Abdomen 2 Circumference in cm
# Var 5 Hip Circumference in cm
# Var 6 Thigh Circumference in cm
# Var 7 Extended Bicep Circumference in cm

plot(bodyfat)
# Good predictors for bodyfat
# lots of multi-collinearity between indepedent variable
cor(bodyfat)
# correlation table backs this concern up and shows 1-to-1 collinearities
# >90% definitely a concern
# 80%-90% still a concern, particularly high 80s
# < 80% not a concern

modelFull <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(modelFull)
# Chest and Hip have insignificant t-tests despite having strong correlations with BodyFat
# This issue is the result of multicollinearity
# Weight and Hip have the strongest correlation so try a model without each

# w/o Weight
model <- lm(BodyFat ~ Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model)
# the R^2 going down only ~2% isn't terrible

# w/o Hip
model <- lm(BodyFat ~ Weight + Chest + Abdomen + Thigh + Biceps, data = bodyfat)
summary(model)
# Hip has a better R^2 so all other considerations being equal, lets keep Hip

# The next strongest correlation is Chest-Abdomen, lets test dropping each variable
# w/o Chest & Dropped Hip
model <- lm(BodyFat ~ Abdomen + Weight + Thigh + Biceps, data = bodyfat)
summary(model)

# w/o Abdomen & Dropped Hip
model <- lm(BodyFat ~ Chest + Weight + Thigh + Biceps, data = bodyfat)
summary(model)
# Drop Chest, the R^2 is much better with Abdomen compared to with Chest
# Also drop Biceps because of its poor t-test

# w/o Chest/Weight/Thigh/Biceps
model <- lm(BodyFat ~ Abdomen + Weight + Thigh, data = bodyfat)
summary(model)

# Experimenting with dropping Thigh, it has the highest P-value
# # w/o Chest/Weight/Thigh/Biceps/Thigh
model <- lm(BodyFat ~ Abdomen + Weight, data = bodyfat)
summary(model)

# VIF 
install.packages("car")
library(car)

modelFull <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(modelFull)

vif(modelFull)
# vif is a function in the car package

# Taking weight out because it has the highest VIF
model <- lm(BodyFat ~ Chest + Abdomen + Hip + Thigh + Biceps, data = bodyfat)
summary(model)
vif(model)
# Biceps next based on t-test, once VIF values are below 10, they rarely go back above 10

#--------------------------
# 7.5 - Predicting Wage
# Add Variable Descriptions

summary(Wages)
plot(Wages)
cor(Wages)

# Converting appropriate Variables to Levels of Factors
Wages$RACE <- as.factor(Wages$RACE)
Wages$OCCUPATION <-  as.factor(Wages$OCCUPATION)
Wages$SECTOR <- as.factor(Wages$SECTOR)
# We can leave the binary variables as in b/c they are already in the form of a dummy variable

modelFull <- lm(WAGE ~ EDUCATION + SOUTH + SEX + EXPERIENCE + UNION + AGE + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
summary(modelFull)

# First look at Wage itself
hist(Wages$WAGE, breaks = 20)
hist(log(Wages$WAGE), breaks = 20)
# Taking log of WAGE pulls in outliers and makes the distribution much closer to normal

modelFull <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + EXPERIENCE + UNION + AGE + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
summary(modelFull)

# What other variables similarly could benefit from a log, 
# aka what other variables have diminishing returns...Experience
modelFull <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE) + UNION + AGE + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
# throws an error because some observations have 0 experience so we have to add a bit to your transformation
modelFull <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE+1) + UNION + AGE + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
summary(modelFull)

#Age could use a log, maybe
modelFull <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE+1) + UNION + log(AGE) + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
summary(modelFull)
# NOPE, R^2 went down a touch, maybe Education

modelFull <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + log(EXPERIENCE+1) + UNION + AGE + RACE + OCCUPATION + SECTOR + MARR, data = Wages) 
summary(modelFull)
# not enough of an improvement to justify keeping transformation

# Now we can start to prune model
# Lets Start with Married
model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + log(EXPERIENCE+1) + UNION + OCCUPATION + SECTOR, data = Wages) 
summary(model)
# If you keep 1 level of a multi-level factor, keep all levels
# So we will keep all 6 levels of Occupation

# Should we keep Sector, it passes the t-test at alpha level 0.05 
# Additionally we may want to see the impact Sector has on Wage

# Managers are 1 in Occupation so we can see they make more than other Occupations

# The order of operations to take away here is that:
# We look at transformations BEFORE pruning the model.
# That way we avoid removing variables that are useful when transformed.
