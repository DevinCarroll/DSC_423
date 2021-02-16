# Starting with the housing dataset
summary(housing)

hist(housing$median_house_value)

hist(housing$median_income)

hist(housing$housing_median_age, breaks = 20)

hist(housing$total_bedrooms)

cor(housing)
# causes a problem because it needs variables that are numeric

cor(housing[,3:9])
# finds the correlation for all rows but only columns 3-9
# creates a correlation table of the each variable with one another
# NA in total_bedrooms are due to prescence of NAs in dataset

cor(housing[,3:9], use='complete.obs')
# use='complete.obs' tells the function to only use complete observations

# after reviewing the correlation table and finding some variables with good correlation
# we should look at those variables to gain a bit more insight
plot(housing$housing_median_age, housing$median_house_value)
plot(housing$total_rooms, housing$median_house_value)
plot(housing$total_bedrooms, housing$median_house_value)
plot(housing$population, housing$median_house_value)
plot(housing$households, housing$median_house_value)
plot(housing$median_income, housing$median_house_value)
#this last  plot (median income/house value) shows much stronger/clear correlation

m1 <- lm(median_house_value ~ housing_median_age, data = housing)
summary(m1)

m2 <- lm(median_house_value ~ total_rooms, data = housing)
summary(m2)

m3 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m3)

m4 <- lm(median_house_value ~ population, data = housing)
summary(m4)

m5 <- lm(median_house_value ~ households, data = housing)
summary(m5)

m6 <- lm(median_house_value ~ median_income, data = housing)
summary(m6)

#--Video 3.5--------------------------------
m7 <- lm(median_house_value ~ housing_median_age + population + households + median_income, data = housing)
summary(m7)

m8 <- lm(median_house_value ~ housing_median_age + population + households + median_income + population*median_income, data = housing)
summary(m8)
# this model barely improves the adjusted R-squared so we can drop the interaction term

m9 <- lm(median_house_value ~ housing_median_age + population + households + median_income + median_income^2, data = housing)
summary(m9)
# this (above) is the wrong way to do a 2nd Order Model

m9 <- lm(median_house_value ~ housing_median_age + population + households + median_income + I(median_income^2), data = housing)
summary(m9)
# this (above) is a better way to do a 2nd Order term, note the I(variable) to specifiy that it is independent

help(I)

m9 <- lm(median_house_value ~ housing_median_age + population + households + median_income + poly(median_income,2), data = housing)
summary(m9)
# this is probably the right way to do this, but for now we will use a simpler solution

# For now, we will instead add a column/variable to the data that is the square of the median_income 
housing$mi2 <- housing$median_income^2
m9 <- lm(median_house_value ~ housing_median_age + population + households + median_income + mi2, data = housing)
summary(m9)

m10 <- lm(median_house_value ~ housing_median_age + population + households + median_income + ocean_proximity, data = housing)
summary(m10)
