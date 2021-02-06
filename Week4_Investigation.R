# These are the three packages the code uses
library(tidyr)
library(dplyr)
library(corrplot)

MinutesMin <- filter(CompleteNBA_GroupProject_V5, MP >= 100)
hist(MinutesMin$weight_kg)
hist(MinutesMin$Age)

cor(CompleteNBA_GroupProject_V5$PTS, CompleteNBA_GroupProject_V5$AST.)

Cat2 <- select(MinutesMin, Player, Year, PTS, USG., FG.,  ORB., ORB, DRB., DRB, TRB., TRB, STL, BLK., BLK, DWS, DBPM, PFouls, height_cm, weight_kg)
Cat4 <- select(MinutesMin, Player, Year, PTS, FG., Age, height_cm, weight_kg, draft_round, draft_pick, true_draft, PG, SG, SF, PF, C)
CatN <- select(MinutesMin, Player, Year, PTS, FG., TS., eFG.)


Cat2$ORB_M <- Cat2$ORB/Cat2$MP
Cat2$DRB_M <- Cat2$DRB/Cat2$MP
Cat2$TRB_M <- Cat2$TRB/Cat2$MP

Cat2_N <- Cat2[,-c(1,2)]
Cat4_N <- Cat4[,-c(1,2)]
CatN_N <- CatN[,-c(1,2)]

cor(CatN_N, use = 'complete.obs')
cor(Cat2_N, use = 'complete.obs')
# r = 0.40 - 0.49: ORB./ORB/TRB./TRB/BLK
# r = 0.30 - 0.39: DRB./DRB/BLK./DWS/DBPM/PFouls
# r = 0.16 = STL

cor(Cat4_N, use = 'complete.obs')
# Height & Weight: r + ~ 0.45 | Everything else is super weak


# Histograms
help(hist)
hist(Cat2_N$BLK.)
hist(Cat4_N$height_cm)

# Cat2 Plots
plot(Cat2$ORB., Cat2$FG.) # Non relationship
plot(Cat2$ORB, Cat2$FG.)  # Maybe kind of relationship, tapered triangle 
plot(Cat2$ORB_M, Cat2$FG.)
plot(Cat2$DRB., Cat2$FG.) # Non relationship
plot(Cat2$DRB, Cat2$FG.)  # blob in the middle
plot(Cat2$TRB., Cat2$FG.) # blob in the middle
plot(Cat2$TRB, Cat2$FG.)  # Fairly Linear Weak Positive Relationship

plot(Cat2$STL, Cat2$FG.)  # Maybe kind of relationship, tapered triangle 
plot(Cat2$BLK., Cat2$FG.) # Non relationship
plot(Cat2$BLK, Cat2$FG.)  # Maybe kind of relationship, tapered triangle 

plot(Cat2$DWS, Cat2$FG.)  # Kind of Linear Positive Relationship, tapered triangle 
plot(Cat2$DBPM, Cat2$FG.) # Non relationship

plot(Cat2$PFouls, Cat2$FG.)
# Weak Positive Linear Relationship

# Cat4 Plots
plot(Cat4$height_cm, Cat4$FG.) 
# split into columns but we can see that taller players have a larger range of FG% than shorter players

plot(Cat4$weight_kg, Cat4$FG.)
# if it is linear it is a very weak relationship

plot(Cat4$draft_round, Cat4$FG)
#not helpful

plot(Cat4$draft_pick, Cat4$FG)
# also not super helpful

plot(Cat4$true_draft, Cat4$FG)
# if it is linear it is a very weak relationship

#----------
# Testing Some Models
help(lm)
model1 <- lm(FG. ~ TRB, data = Cat2)
summary(model1) # Adjusted R^2 of 0.1913 test is significant

model2 <- lm(FG. ~ height_cm, data = Cat4)
summary(model2) # Adjusted R^2 of 0.2037, test is significant

model3 <- lm(FG. ~ weight_kg, data = Cat4)
summary(model3) # Adjusted R^2 of 0.2151, test is significant

model4 <- lm(FG. ~ true_draft, data = Cat4)
summary(model4) # test is significant but adjusted R^2 is useless

model11 <- lm(FG. ~ TRB_M + ORB_M + height_cm + weight_kg, data = Cat2_N)
summary(model11) # Adjusted R^2 of 0.3566, test is significant


#------------Post 7pm
plot(Cat4$Year, Cat4$height_cm)

centerHeight <- filter(Cat4, C == 1)
summary(centerHeight)
plot(centerHeight$height_cm, centerHeight$FG.)
#------------------
centers <- filter(CompleteNBA_GroupProject_V5, C == 1)
centerSummary <- summary(centers)
centerSummary

#-----------
yearlyBreakdown <- MinutesMin %>%
  group_by(Year) %>%
  summarize(NumPlayers = n(),
            FGp_Mean = mean(FG., na.rm = T),
            FGp_STD = sd(FG., na.rm = T),
            H_Mean = mean(height_cm, na.rm = T),
            H_STD = sd(height_cm, na.rm = T),
            W_Mean = mean(weight_kg, na.rm = T),
            W_STD = sd(weight_kg, na.rm = T),
            TRB_Mean = mean(TRB, na.rm = T),
            TRB_STD = sd(TRB, na.rm = T),
            ORB_Mean = mean(ORB, na.rm = T),
            ORB_STD = sd(ORB, na.rm = T),
            DRB_Mean = mean(DRB, na.rm = T),
            DRB_STD = sd(DRB, na.rm = T))

#------------- Work on Variables worth including in Report

boxplot(Cat2$height_cm, na.rm = T)
quantile(Cat2$height_cm, na.rm = T)
#shortie <- filter(Cat2, height_cm < 170)

boxplot(Cat2$weight_kg, na.rm = T)
quantile(Cat2$weight_kg, na.rm = T)
#outliersW <- filter(Cat2, weight_kg >= 139 | weight_kg <= 61)


cor(Cat2$FG., Cat2$ORB.)
boxplot(Cat2$ORB., na.rm = T)
quantile(Cat2$ORB., na.rm = T)
outliersORBp <- filter(Cat2, ORB. > 13.25)

cor(Cat2$FG., Cat2$ORB)
boxplot(Cat2$ORB, na.rm = T)
quantile(Cat2$ORB, na.rm = T)
outliersORB <- filter(Cat2, ORB > 156.5)

cor(Cat2$FG., Cat2$ORB_M)
boxplot(Cat2$ORB_M, na.rm = T)
quantile(Cat2$ORB_M, na.rm = T)
outliersORB_M <- filter(Cat2, ORB_M > 0.1155496)

cor(Cat2$FG., Cat2$TRB.)
boxplot(Cat2$TRB., na.rm = T)
quantile(Cat2$TRB., na.rm = T)
outliersTRBp <- filter(Cat2, TRB. > 19.55)

cor(Cat2$FG., Cat2$TRB)
boxplot(Cat2$TRB, na.rm = T)
quantile(Cat2$TRB, na.rm = T)
outliersTRB <- filter(Cat2, TRB > 544.5)




