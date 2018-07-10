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

#running 4 variable lm 
model1 <- lm(totViewers ~ month + medianViewsPerMatchUp + bestRankAmongTeams + gameType)

summary(model1)

aov(formula = model1)
plot(model1)


#running two attribute model
model2 <- lm(totViewers ~ month + medianViewsPerMatchUp)

summary(model2)
aov(model2)
plot(model2)

#running 3 attribute model
model3 <- lm(totViewers ~ month + medianViewsPerMatchUp + gameType)
summary(model3)
aov(model3)
plot(model3)

newpt <- data.frame(month = partitionedTraining$Month[2], medianViewsPerMatchUp = partitionedTraining$Median_Views_Per_Matchup[2], bestRankAmongTeams = partitionedTraining$bestRankAmongTeams[2], gameType = partitionedTraining$gameType[2], se.fit = T)
predict(model1, newdata = newpt)

  