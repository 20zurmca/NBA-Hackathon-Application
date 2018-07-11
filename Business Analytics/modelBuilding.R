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
testData <- read.csv("testingWithAttributes.csv", header = T)


############################################################ BUILDING 3 MODELS #################################################

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


#Generate model 1 fitted values
for(i in 1:length(partitionedTesting$Game_ID))
{
  newpt <- data.frame(month = partitionedTesting$Month[i], medianViewsPerMatchUp = partitionedTesting$Median_Views_Per_Matchup[i], bestRankAmongTeams = partitionedTesting$bestRankAmongTeams[i], gameType = partitionedTesting$gameType[i])
  partitionedTesting$modelOnePredictions[i] <- predict(model1, newdata = newpt)
  partitionedTesting$modelOneDeviation[i] <- abs((partitionedTesting$Tot_Viewers[i] - partitionedTesting$modelOnePredictions[i])/partitionedTesting$Tot_Viewers[i])
}

mean(partitionedTesting$modelOneDeviation)

#Generate model 2 fitted values
for(i in 1:length(partitionedTesting$Game_ID))
{
  newpt <- data.frame(month = partitionedTesting$Month[i], medianViewsPerMatchUp = partitionedTesting$Median_Views_Per_Matchup[i])
  partitionedTesting$modelTwoPredictions[i] <- predict(model2, newdata = newpt)
  partitionedTesting$modelTwoDeviation[i] <- abs((partitionedTesting$Tot_Viewers[i] - partitionedTesting$modelTwoPredictions[i])/partitionedTesting$Tot_Viewers[i])
}

mean(partitionedTesting$modelTwoDeviation) #MAPE

#Generate model 3 fitted values
for(i in 1:length(partitionedTesting$Game_ID))
{
  newpt <- data.frame(month = partitionedTesting$Month[i], medianViewsPerMatchUp = partitionedTesting$Median_Views_Per_Matchup[i], gameType = partitionedTesting$gameType[i])
  partitionedTesting$modelThreePredictions[i] <- predict(model3, newdata = newpt)
  partitionedTesting$modelThreeDeviation[i] <- abs((partitionedTesting$Tot_Viewers[i] - partitionedTesting$modelThreePredictions[i])/partitionedTesting$Tot_Viewers[i])
}

mean(partitionedTesting$modelThreeDeviation) #MAPE


#--------------------------------------------------------------------------------------------------------------------------#

#KNN REGRESSION

standardize <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize <- function(x)
{
  return ((x - mean(x))/sd(x))
}


#Trying to find the value of k for the lowest MAPE

getMinMAPE <- function()
{
  MAPE <- NULL
  trainingAcutal <- partitionedTraining[,"Tot_Viewers"]
  
  for(i in 1: 600)
  {
    knnDeviation <- NULL
    knnPredictions <- FNN::knn.reg(trainingPredictors, testingPredictors, trainingAcutal, k= i)
   for(j in 1: 600)
    {
      knnDeviation[j] <- abs((partitionedTesting$Tot_Viewers[j] - knnPredictions$pred[j])/partitionedTesting$Tot_Viewers[j])
    }
    MAPE[i] <- mean(knnDeviation)
  }
  return (min(MAPE))
}


#format: knn.reg(predictors from training data, predictors from testing data, actual training numerical outcomes, k = 5)

#standardizing medianViewsPerMatchup

#MODEL 1: ALL ATTRIBUTES
trainingPredictors <- (partitionedTraining[,c("Month", "Day", "All_Star_Count", "Has_Top_Five_Player", "Time_Zone_Of_Game", "Median_Views_Per_Matchup", "Is_Sat_or_Sun", "bestRankAmongTeams")])
trainingPredictors <- cbind(lapply(trainingPredictors[,c("Day", "Has_Top_Five_Player", "Time_Zone_Of_Game", "Is_Sat_or_Sun")], as.numeric), trainingPredictors)
trainingPredictors <- trainingPredictors[,c(-6,-8,-9,-11)]

normlizedTraining <- as.data.frame(lapply(trainingPredictors[1:8], normalize))


testingPredictors <- partitionedTesting[,c("Month", "Day", "All_Star_Count", "Has_Top_Five_Player", "Time_Zone_Of_Game", "Median_Views_Per_Matchup", "Is_Sat_or_Sun", "bestRankAmongTeams")]
testingPredictors <- cbind(lapply(testingPredictors[,c("Day", "Has_Top_Five_Player", "Time_Zone_Of_Game", "Is_Sat_or_Sun")], as.numeric), testingPredictors)
testingPredictors <- testingPredictors[,c(-6,-8,-9,-11)]

normalizedTesting <- as.data.frame(lapply(testingPredictors[1:8], normalize))



trainingAcutal <- partitionedTraining[,"Tot_Viewers"]


#MODEL 2: 


trainingPredictors <- (partitionedTraining[,c("Month", "Median_Views_Per_Matchup", "Is_Sat_or_Sun", "bestRankAmongTeams", "gameType")])
trainingPredictors <- cbind(lapply(trainingPredictors[,1:5], as.numeric), trainingPredictors)
trainingPredictors <- trainingPredictors[,c(-6,-7,-8,-9,-10)]

normlizedTraining <- as.data.frame(lapply(trainingPredictors[1:5], standardize))


testingPredictors <- (partitionedTesting[,c("Month", "Median_Views_Per_Matchup", "Is_Sat_or_Sun", "bestRankAmongTeams", "gameType")])
testingPredictors <- cbind(lapply(testingPredictors[,1:5], as.numeric), testingPredictors)
testingPredictors <- testingPredictors[,c(-6,-7,-8,-9,-10)]


normalizedTesting <- as.data.frame(lapply(testingPredictors[1:5], standardize))



trainingAcutal <- partitionedTraining[,"Tot_Viewers"]

getMinMAPE()

#MODEL 3: 


trainingPredictors <- (partitionedTraining[,c("Month", "Median_Views_Per_Matchup", "bestRankAmongTeams", "gameType", "All_Star_Count")])
trainingPredictors <- cbind(lapply(trainingPredictors[,1:5], as.numeric), trainingPredictors)
trainingPredictors <- trainingPredictors[,1:5]

normlizedTraining <- as.data.frame(lapply(trainingPredictors[1:5], standardize))


testingPredictors <- (partitionedTesting[,c("Month", "Median_Views_Per_Matchup" , "bestRankAmongTeams", "gameType", "All_Star_Count")])
testingPredictors <- cbind(lapply(testingPredictors[,1:5], as.numeric), testingPredictors)
testingPredictors <- testingPredictors[,1:5]


normalizedTesting <- as.data.frame(lapply(testingPredictors[1:5], standardize))



trainingAcutal <- partitionedTraining[,"Tot_Viewers"]

getMinMAPE()

#MODEL 4: 


trainingPredictors <- (partitionedTraining[,"Median_Views_Per_Matchup"])

#normlizedTraining <- as.data.frame(lapply(trainingPredictors[1], standardize))


testingPredictors <- (partitionedTesting[,"Median_Views_Per_Matchup"])



#normalizedTesting <- as.data.frame(lapply(testingPredictors[1], standardize))


getMinMAPE()

