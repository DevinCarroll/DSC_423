# 4.4
#----------------------
summary(kc_house_data)
d <- kc_house_data[,-c(1,2)]
# the above line removes columns 1 and 2 from kc_house_data and stores them in d
# d is now a copy of kc_house_data with the first 2 columns missing

plot(kc_house_data$long, kc_house_data$lat)
# creates a rough map of King County! but not useful for regression

#---------------
d <- kc_house_data[,-c(1,2,7,13, 14, 17,18,19)]
# now additionally remove lat and long and zip code and sqr basement and sqr lot and sqr above

model1 <- lm(price ~ ., data = d)
# the period after the ~ says use all other variables in the data frame
summary(model1)

cor(d)
#correlation table for d

help(sample)
# ^^^ Read Me ^^^

dsample <- d[sample(1:nrow(d), 1000, replace=F),]
# takes 1000 samples from the rows of d and includes all columns, note the, after the sample command
plot(dsample)
# the goal here is to visually inspect scatter plots for variables that may benefit from 2nd Order Terms

#--------------------------
#Adding 2nd Order Terms
d$sqft_livingSQ <- (d$sqft_living)^2
d$sqft_lotSQ <- (d$sqft_lot/1000)^2
# we add the '/1000' because some of the values are larger than what an integer type can hold
d$wat_bed <- d$waterfront * d$bedrooms
# this is how we create an interaction term


model <- lm(price ~ ., data = d)
# the period after the ~ says use all other variables in the data frame
summary(model)

cor(d)
#correlation table for d

# 4.5
#-----------------------------------

d <- kc_house_data[,-c(1,2,7,13, 14, 17,18,19)]
# now additionally remove lat and long and zip code and sqr basement and sqr lot and sqr above

model1 <- lm(price ~ ., data = d)
# the period after the ~ says use all other variables in the data frame
summary(model1)

#Adding 2nd Order Terms
d$sqft_livingSQ <- (d$sqft_living)^2
d$sqft_lotSQ <- (kc_house_data$sqft_lot/1000)^2
# we add the '/1000' because some of the values are larger than what an integer type can hold
model2 <- lm(price ~ ., data = d)
summary(model2)

#Adding an Interaction Term
d$wat_bed <- d$waterfront * d$bedrooms
model3 <- lm(price ~ ., data = d)
summary(model3)

# The Improvement from Model2 to Model3, is adding that term worth it?
# JFG says no.

#Interactions: binary variables, bedrooms/bathrooms
#2nd Order: Grade

d$floors_sqrft <- d$floors*d$sqft_living
model4 <- lm(price ~ ., data = d)
summary(model4)
# 0.0049 improvement in Adjusted R^2, but only 0.0010 if grade^is already included
d$floors_sqrft = NULL

d$grade_sq <- (d$grade)^2
model5 <- lm(price ~ ., data = d)
summary(model5)
# 0.0095 improvement in Adjusted R^2

d$bed_bath <- d$bedrooms * d$bathrooms
model6 <- lm(price ~ ., data = d)
summary(model6)
# bed_bath interaction term isn't significant enough to keep
d$bed_bath = NULL

d$view_lot <- d$view * d$sqft_lot15
model7 <- lm(price ~ ., data = d)
summary(model7)
d$view_lot = NULL

d$view_bed <- d$view*d$bedrooms
model8 <- lm(price ~ ., data = d)
summary(model8)
d$view_bed = NULL

d$view_water <- d$view * d$waterfront
model9 <- lm(price ~ ., data = d)
summary(model9)
d$view_water = NULL

d$bath_sq <- (d$bathrooms)^2
model10 <- lm(price ~ ., data = d)
summary(model10)
d$bath_sq = NULL

d$sqft_living15_sq <- (d$sqft_living15)^2
model11 <- lm(price ~ ., data = d)
summary(model11)
d$sqft_living15_sq = NULL

d$condition_sq <- (d$condition^2)
model12 <- lm(price ~ ., data = d)
summary(model12)
d$condition_sq = NULL

#------5.1
set.seed(123)
# sets the seed

runif(1)
# random number between 0-1
#-------------
set.seed(123)

d <- kc_house_data[,-c(1,2)]

partition <- sample(2, nrow(d), replace=T, prob = c(0.80, 0.20))
train <- d[partition == 1, ]
test <- d[partition == 2, ]

model <- lm(price ~ ., data = train)

prediction <- predict(model, test)
actual = test$price

cor(prediction, actual)
plot(prediction, actual)


#------------------
#install.packages("DAAG")
#install.packages("latticeExtra")
#install.packages("https://cran.r-project.org/src/contrib/Archive/DAAG/DAAG_1.22.tar.gz",repos=NULL, type="source")

library(latticeExtra)
library(DAAG)

help(cv.lm)
# help for cross-validation for linear regression

# 3 Fold Cross Validation
out <- cv.lm(data = d, form.lm = formula(price ~ .), plotit = 'Observed', m=3)

#---- 5.2
# Stepwise Regression
# install.packages("MASS")
library(MASS)

help(stepAIC)

#---------------
d <- kc_house_data[,-c(1,2)]
# Backward Elimination
model_full <- lm(price ~ ., data = d)

step <- stepAIC(model_full, direction = 'backward')                 
step$anova  #displays results

#------------------
# Forward Selection
d <- kc_house_data[,-c(1,2)]
model_full <- lm(price ~ ., data = d)
model_empty <- lm(price ~ 1, data = d)

step <- stepAIC(model_empty, direction = 'forward', scope = list(upper=model_full, lower=model_empty))
summary(step)
