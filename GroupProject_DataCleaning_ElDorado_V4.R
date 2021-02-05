# In this script the 1950-2017 NBA Dataset is Season_Stats
# and the 1999-2020 International Data Set is InternationalBasketballStats

# These are the two packages the code uses
library(tidyr)
library(dplyr)

# P1
# The 1st task is remove non-NBA plays and non-Regular Season observations:
# This is actually only 1 line because only NBA games have the Stage as Regular_Season
cleaned_InternationalBasketballStats <- filter(InternationalBasketballStats, Stage == "Regular_Season")
# We want to this shortening to remove redundant observers later as well as non-revelavant ones.
# Notice now the variable we want to work with is cleaned_InternationalBasketballStats

# P3
# Now we are going to clean up the year column in the International dataset.
#Split subset Season into Start and Year
cleaned_InternationalBasketballStats <- separate(cleaned_InternationalBasketballStats,Season,into=c('Start','Year'),sep="-")
#Convert split column "Year" to integers
cleaned_InternationalBasketballStats$Year <-as.integer(cleaned_InternationalBasketballStats$Year)

# P2
# Next we are going to create the true_draft column which presents the overall draft pick this player was selected in
# First we have to split the dataset into 2003 and earlier, when there were 29 teams
# and 2004 and later when there were 30 teams
# adjust each data accordingly
pre2004 <- filter(cleaned_InternationalBasketballStats, Year <= 2003)
pre2004$true_draft <- (pre2004$draft_round-1)*29 + pre2004$draft_pick

post2004 <- filter(cleaned_InternationalBasketballStats, Year > 2003)
post2004$true_draft <- (post2004$draft_round-1)*30 + post2004$draft_pick

# then recombine them
cleaned_InternationalBasketballStats <- bind_rows(pre2004, post2004)

# P4
# Next we going to narrow down the International dataset to only the columns we need from this dataset
# This will avoid redundant variables and make the merging process smoother.
cleaned_InternationalBasketballStats <- select(cleaned_InternationalBasketballStats, Year, Player, height_cm, weight_kg, draft_round, draft_pick, true_draft)

# P5
# Now we can merge the two datasets:
# We are calling the new dataset: full_NBA
full_NBA <- left_join(Seasons_Stats, cleaned_InternationalBasketballStats, by=c("Player","Year"), all.y=TRUE)

# P6
# These commands remove a few unneeded variables in our merged dataset
full_NBA$X3PAr = NULL
full_NBA$X = NULL
full_NBA$blanl = NULL
full_NBA$blank2 = NULL
# We could also remove Tm, I haven't for now to help us spot errors
#full_NBA$Tm = NULL

# P7
# This copies the PF, personal fouls column into a new PFouls column so PF can be used for the position Power Forward
full_NBA$PFouls <- full_NBA$PF
# These lines initialize our position values
full_NBA$PG <- full_NBA$Age*0
full_NBA$SG <- full_NBA$Age*0
full_NBA$SF <- full_NBA$Age*0
full_NBA$PF <- full_NBA$Age*0
full_NBA$C <- full_NBA$Age*0
#  This is where we need to add code to update/correct those position values
full_NBAV2 <- full_NBA

C <-  filter(full_NBAV2, Pos == "C")
C$C <- 1

C_PF <-  filter(full_NBAV2, Pos == 'C-F' | Pos == 'C-PF' | Pos == 'F-C' | Pos == 'PF-C')
C_PF$C <- 1
C_PF$PF <- 1

SF_PF = filter(full_NBAV2, Pos == 'F'| Pos == 'PF-SF' | Pos == 'SF-PF')
SF_PF$PF <- 1
SF_PF$SF <- 1

PF <- filter(full_NBAV2, Pos == "PF")
PF$PF <- 1

G <- filter(full_NBAV2, Pos == "G" | Pos == "PG-SG" | Pos == 'SG-PG')
G$SG <- 1
G$PG <- 1

C_SF <- filter(full_NBAV2, Pos == "C-SF")
C_SF$C <- 1
C_SF$SF <- 1

PG <- filter(full_NBAV2, Pos == "PG") 
PG$PG <- 1

SG <- filter(full_NBAV2, Pos == "SG")
SG$SG <- 1

SF <- filter(full_NBAV2, Pos == "SF")
SF$SF <- 1

SG_SF <- filter(full_NBAV2, Pos == "SF-SG" | Pos == "SG-SF" | Pos == 'SG-PF')
SG_SF$SG <- 1
SG_SF$SF <- 1

PG_SF <- filter(full_NBAV2, Pos == "PG-SF" | Pos == "SF-PG")
PG_SF$PG <- 1
PG_SF$SF <- 1

G_F <- filter(full_NBAV2, Pos == "F-G" | Pos == "G-F")
G_F$SG <- 1
G_F$SF <- 1

empty <- filter(full_NBAV2, Pos == '')


# And now we rebind the rows together use tidyr's bind_rows
full_NBAV4 <- bind_rows(C_PF, C_SF, C, G, PF, PG, PG_SF, SF, SF_PF, SG, SG_SF, G_F, empty)

full_NBA <- full_NBAV4

# and finally this line will write the updated dataframe into a CSV for later use
# you will need to update the file path for your computer however
write.csv(full_NBA, 
          'C:\\Users\\devca\\OneDrive\\Desktop\\DSC 423\\Group Project\\CompleteNBA_GroupProject_V4.csv', row.names =  T)

