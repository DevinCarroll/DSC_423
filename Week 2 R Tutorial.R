# Scatter Plot of Car weight(x) and Car MPG (y)
plot(cars$weight, cars$mpg)

# Scatter plot of Horsepower and MPG
plot(cars$horsepower, cars$mpg)


# All Plots for Cars
plot(cars)
# The Resulting plot will display all scatter plots with each combination of variables
# You will get A on the axis by B on the Y axis and vice versa
# Do you can tell which variable is on which axis by referencing the labels along the diagonal
# For example, with cars:
# Every plot above/below the MPG box has MPG along the x-axis
# Every plot to the left/right of the MPG box, has MPG along the y-axis

# Slicing and Dicing
# R uses [row, column]
# Removing the Model Info
numOnly <- cars[, 2:7]

plot(numOnly)


# Building a Regression Model in R
# Use lm: stands for linear model
# help(lm)

model <- lm(cars$mpg ~ cars$displacement) # the middle symbol in () is the tilda
# General syntax: lm(dataFrame$response_variable ~ dataFrame$explanatory_variable)
# Think of the tilde as "approximately equal to sign"
summary(model)

model2 <- lm(mpg ~ displacement, data = cars)
# Alternative Syntax: lm(response_variable ~ explanatory_variable, data = dataFrame)

coef(model) # coefficients(model) works too
# Displays Beta 0 and Beta 1 of the Linear LSR Model for the 2 Variables

fitted(model)
# displays predicted values for each potential data point of the response variable

resid(model) # residuals(model) works too
# displays the residual for each data point

# Building several models and comparing them
model1 <- lm(cars$mpg ~ cars$displacement)
summary(model1)
model1R2 <- 0.6467

model2 <- lm(cars$mpg ~ cars$horsepower)
summary(model2)
model2R2 <- 0.6059

model3 <- lm(cars$mpg ~ cars$weight)
summary(model3)
model3R2 <- 0.6918

model4 <- lm(cars$mpg ~ cars$acceleration)
summary(model4)
model4R2 <- 0.1766

model5 <- lm(cars$mpg ~ cars$cylinders)
summary(model5)
model5R2 <- 0.6012