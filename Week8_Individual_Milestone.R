# Week 8 Individal Milestone
# Start by trying to have a variable for player's points from the previous season
library(DAAG)
library(latticeExtra)
library(MASS)
library(car)
library(tidyr)
library(dplyr)

#Changes DataFrame into simpler variable name
fullData <- CompleteNBA_GroupProject_V6
fullData_Complete <- fullData %>% drop_na() 

#Sorting/Arranging Testing
help(arrange)
# This arranges the data by Player name then Year
fullData_ordered <- arrange(fullData, Player, Year)
# This simplifies the process of creating the Previous Year Points variable
# Now PTS can be used with lag and some condition checking
help(lag)
testFrame <- select(fullData_ordered, Player, Year, PTS)
#creating a reduced data frame to more easily check this process is working
testFrame$Prev_PTS <- testFrame$PTS
#create a new variable for Previous Season's Points using PTS
testFrame$Prev_PTS <- lag(testFrame$Prev_PTS)
# This moves all values down 1 row
# So now the key is to remove each player's first occurrence(?)
testFrameV2 <- slice(group_by(testFrame, Player), -1)
# and I think that worked