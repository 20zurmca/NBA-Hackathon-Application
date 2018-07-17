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

############################################################ HELPER FUNCTIONS ##########################################################################

getHelperQuery <- function(home, away)
{
  
  return(sqldf(paste0("select Tot_Viewers from totalViewersPerGame where Home_Team ='", home, "'and Away_Team = '", away, "'"))$Tot_Viewers)
}

########################################################## IMPORTING DATA ####################################################################################################
#importing new training data that was made in BusinessAnalytics.R
newTraining <- read.csv("newTraining.csv", header = T)
testData <- read.csv("testingWithAttributes.csv", header = T)
gameData <- read.csv("game_data.csv", header = T)
rankings2015_2016 <- read.csv("rankings2015_2016.csv", header = T)
rankings2016_2017 <- read.csv("rankings2016_2017.csv", header = T)

teamRankings <- data.table(gameData, key = "Game_Date")
teamRankings <- teamRankings[,transform(.SD, teamRanking = rank(-Wins_Entering_Gm, ties.method = "min")), by = Game_Date]
teamRankings <- teamRankings[,c("Game_Date", "Team", "Wins_Entering_Gm", "teamRanking")]



############################################################ BUILDING MODEL #################################################

#First partitioning the data

newTraining <- newTraining[-c(1014), ] #Removing one overly influential point (high cook's distance >0.5)

sample <- sample.int(n=nrow(newTraining), size = floor(.70 * nrow(newTraining)), replace = FALSE, prob = NULL)
partitionedTraining <- newTraining[sample, ]
partitionedTesting <- newTraining[-sample, ]


newTraining$Day <- as.factor(newTraining$Day)

month <- newTraining$Month
day   <- newTraining$Day
gameType <- newTraining$gameType
All_Star_Count <- newTraining$All_Star_Count
isLebron <- newTraining$Is_Lebron_Playing
hasTopFive <- newTraining$Has_Top_Five_Player
timeZone <- newTraining$Time_Zone_Of_Game
isWeekend <- newTraining$Is_Sat_or_Sun
bestRankAmongTeams <- newTraining$bestRankAmongTeams
medianViewsPerMatchUp <- newTraining$Median_Views_Per_Matchup

totViewers <- newTraining$Tot_Viewers


#Assigning less weights to outliers
newTraining$weights <- NULL
for(i in 1:length(newTraining$Game_ID))
{
  newTraining$weights[i] <- 1/(newTraining$Tot_Viewers[i])^2
}


#running 4 variable lm 
model1 <- lm(totViewers ~ month + medianViewsPerMatchUp + bestRankAmongTeams + gameType, weights = newTraining$weights)


summary(model1)

aov(formula = model1)
plot(model1)


#Generate model 1 fitted values, ignore the warning because the testing data is not same length as training
for(i in 1:length(partitionedTesting$Game_ID))
{
  newpt <- data.frame(month = partitionedTesting$Month[i], medianViewsPerMatchUp = partitionedTesting$Median_Views_Per_Matchup[i], bestRankAmongTeams = partitionedTesting$bestRankAmongTeams, gameType = partitionedTesting$gameType[i])
  partitionedTesting$modelOnePredictions[i] <- predict(model1, newdata = newpt)
  partitionedTesting$modelOneDeviation[i] <- abs((partitionedTesting$Tot_Viewers[i] - partitionedTesting$modelOnePredictions[i])/partitionedTesting$Tot_Viewers[i])
}


#Answer the testing data 
for(i in 1:length(testData$Game_ID))
{
  newpt <- data.frame(month = testData$Month[i], medianViewsPerMatchUp = testData$Median_Views_Per_Matchup[i], bestRankAmongTeams = testData$bestRankAmongTeams[i], gameType = testData$gameType[i])
  testData$Total_Viewers[i] <- predict(model1, newdata = newpt)
}

write.csv(testData, "testDataSolutionModel1.csv")



 #--------------------------------------------------------------------------------------------------------------------------#

#KNN REGRESSION - was not a good model

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

