library(sqldf)
library(lubridate)
library(stringi)
library(data.table)

getwd()
#setwd("ProjectRepos/NBAH18/Business\ Analytics")
setwd("/home/cameron/NBAH18/Business\ Analytics")
source("functions.R")

#Before pushing, comment out my working directory and uncomment yours

#setwd("ProjectRepos/NBAH18/Business\ Analytics")

########################################################## IMPORTING DATA ####################################################################################################
#importing new training data that was made in BusinessAnalytics.R
newTraining <- read.csv("newTraining.csv", header = T)


############################################################ BUILDING THE MODEL #################################################

#First partitioning the data

sample <- sample.int(n=nrow(newTraining), size = floor(.70 * nrow(newTraining)), replace = FALSE, prob = NULL)
partitionedTraining <- newTraining[sample, ]
partitionedTesting <- newTraining[-sample, ]

partitionedTraining$Day <- as.factor(partitionedTraining$Day)

month <- partitionedTraining$Month
day   <- partitionedTraining$Day
gameType <- partitionedTraining$gameType
All_Star_Count <- partitionedTraining$All_Star_Count
isLebron <- partitionedTraining$Is_Lebron_Playing
hasTopFive <- partitionedTraining$Has_Top_Five_Player
timeZone <- partitionedTraining$Time_Zone_Of_Game
medianViewsPerMatchUp <- partitionedTraining$Median_Views_Per_Matchup
isWeekend <- partitionedTraining$Is_Sat_or_Sun
bestRankAmongTeams <- partitionedTraining$bestRankAmongTeams

totViewers <- partitionedTraining$Tot_Viewers

#running lm 
mreg <- lm(totViewers ~ month + isWeekend + medianViewsPerMatchUp + bestRankAmongTeams + gameType)
summary(mreg)

aov(formula = mreg)
plot(mreg)

#running two attribute model
mregTwo <- lm(totViewers ~ month + medianViewsPerMatchUp)
summary(mregTwo)
aov(mregTwo)

summary(mregTwo)  
plot(mregTwo)

mregThree <- lm(totViewers ~ month + medianViewsPerMatchUp + isLebron )
summary(mregThree)

  