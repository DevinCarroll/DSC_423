# These are the three packages the code uses
library(tidyr)
library(dplyr)
library(corrplot)

ExpSet <- select(CompleteNBA_GroupProject_V5, Player, Year, PTS, USG., GS, G, MP, AST, STL, TOV, TRB, FT, FTA, X2PA, X3PA, FGA)


# Histograms of Variables we are keeping
hist(ExpSet$PTS)
hist(ExpSet$USG.)
hist(ExpSet$GS)
hist(ExpSet$G)
hist(ExpSet$G)
hist(ExpSet$MP)
hist(ExpSet$AST)
hist(ExpSet$STL)
hist(ExpSet$TOV)
hist(ExpSet$TRB)
hist(ExpSet$FT)
hist(ExpSet$FTA)
hist(ExpSet$X2PA)
hist(ExpSet$X3PA)

# Boxplot/5-Number Summary for Variables We are Keeping
boxplot(ExpSet$PTS, na.rm = T)
quantile(ExpSet$PTS, na.rm = T)
outliersPTS <- filter(ExpSet, PTS > 906)

cor(ExpSet$PTS, ExpSet$USG.)
boxplot(ExpSet$USG., na.rm = T)
quantile(ExpSet$USG., na.rm = T)
outliersUSG <- filter(ExpSet, TRB > 28.5 | TRB < 8.1)

cor(ExpSet$PTS, ExpSet$GS)
boxplot(ExpSet$GS, na.rm = T)
quantile(ExpSet$GS, na.rm = T)

cor(ExpSet$PTS, ExpSet$G)
boxplot(ExpSet$G, na.rm = T)
quantile(ExpSet$G, na.rm = T)

cor(ExpSet$PTS, ExpSet$MP)
boxplot(ExpSet$MP, na.rm = T)
quantile(ExpSet$MP, na.rm = T)

cor(ExpSet$PTS, ExpSet$AST)
boxplot(ExpSet$AST, na.rm = T)
quantile(ExpSet$AST, na.rm = T)
outliersAST <- filter(ExpSet, AST > 236.5)

cor(ExpSet$PTS, ExpSet$STL)
boxplot(ExpSet$STL, na.rm = T)
quantile(ExpSet$STL, na.rm = T)
outliersSTL <- filter(ExpSet, STL > 93.5)

cor(ExpSet$PTS, ExpSet$TOV)
boxplot(ExpSet$TOV, na.rm = T)
quantile(ExpSet$TOV, na.rm = T)
outliersTOV <- filter(ExpSet, AST > 168)

cor(ExpSet$PTS, ExpSet$TRB)
boxplot(ExpSet$TRB, na.rm = T)
quantile(ExpSet$TRB, na.rm = T)
outliersTRB <- filter(ExpSet, TRB > 544.5)

cor(ExpSet$PTS, ExpSet$FT)
boxplot(ExpSet$FT, na.rm = T)
quantile(ExpSet$FT, na.rm = T)
outliersFT <- filter(ExpSet, FTA > 201.5)

cor(ExpSet$PTS, ExpSet$FTA)
boxplot(ExpSet$FTA, na.rm = T)
quantile(ExpSet$FTA, na.rm = T)
outliersFTA <- filter(ExpSet, FTA > 205.5)

cor(ExpSet$PTS, ExpSet$X2PA)
boxplot(ExpSet$X2PA, na.rm = T)
quantile(ExpSet$X2PA, na.rm = T)
outliersX2PA <- filter(ExpSet, X2PA > 553.5)

cor(ExpSet$PTS, ExpSet$X3PA)
boxplot(ExpSet$X3PA, na.rm = T)
quantile(ExpSet$X3PA, na.rm = T)
outliersX3PA <- filter(ExpSet, X2PA > 231)
