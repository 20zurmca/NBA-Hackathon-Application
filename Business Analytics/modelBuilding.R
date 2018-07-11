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

rankings2015_2016 <- read.csv("rankings2015_2016.csv", header = T)
rankings2016_2017 <- read.csv("rankings2016_2017.csv", header = T)


#loads the best ranking team
loadBestRankingTeam <- function()
{
  j <- 1;
  while(j <= length(testData$Game_ID))
  {
    #if it's the month of October, use the highest ranking team from the previous season
    if(testData$Month[j] == 10 && getYearFromDate(testData$Game_Date[j]) == 16)
    {
      for(i in 1:length(rankings2015_2016$Rk))
      {
        if((testData$Home_Team[j] == rankings2015_2016[i,3] || testData$Away_Team[j] == rankings2015_2016[i,3]))
        {
          testData$bestRankAmongTeams[j] <- i;
          break;
        }
      }
    } else if(testData$Month[j] == 10 && getYearFromDate(testData$Game_Date[j]) == 17)
    {
      for(i in 1:length(rankings2016_2017$Rk))
      {
        if((testData$Home_Team[j] == rankings2016_2017[i,3] || testData$Away_Team[j] == rankings2016_2017[i,3]))
        {
          testData$bestRankAmongTeams[j] <- i;
          break;
        }
      }
    } else  
    {
      homeTeam <- testData$Home_Team[j]
      awayTeam <- testData$Away_Team[j]
      
      rankHome <- sqldf(paste0("select teamRanking from teamRankings where Game_Date = '", testData$Game_Date[j], "' and Team = '", testData$Home_Team[j], "'"))
      rankAway <- sqldf(paste0("select teamRanking from teamRankings where Game_Date = '", testData$Game_Date[j], "' and Team = '", testData$Away_Team[j], "'"))
      
      if(rankHome$teamRanking > rankAway$teamRanking)
      {
        testData$bestRankAmongTeams[j] <- rankAway$teamRanking
      } else
      {
        testData$bestRankAmongTeams[j] <- rankHome$teamRanking
      }
    }
    j <- j+1
  }
}

loadBestRankingTeam()


############################################################ BUILDING MODEL #################################################

#First partitioning the data

sample <- sample.int(n=nrow(newTraining), size = floor(.70 * nrow(newTraining)), replace = FALSE, prob = NULL)
partitionedTraining <- newTraining[sample, ]
partitionedTesting <- newTraining[-sample, ]

newTraining <- newTraining[-c(1014), ] #Removing one overly influential point (high cook's distance)

newTraining$Day <- as.factor(newTraining$Day)

month <- newTraining$Month
day   <- newTraining$Day
gameType <- newTraining$gameType
All_Star_Count <- newTraining$All_Star_Count
isLebron <- newTraining$Is_Lebron_Playing
hasTopFive <- newTraining$Has_Top_Five_Player
timeZone <- newTraining$Time_Zone_Of_Game
medianViewsPerMatchUp <- newTraining$Median_Views_Per_Matchup
isWeekend <- newTraining$Is_Sat_or_Sun
bestRankAmongTeams <- newTraining$bestRankAmongTeams

totViewers <- newTraining$Tot_Viewers

#running 4 variable lm 
model1 <- lm(totViewers ~ month + medianViewsPerMatchUp + bestRankAmongTeams + gameType)

summary(model1)

aov(formula = model1)
plot(model1)




#Generate model 1 fitted values
for(i in 1:length(partitionedTesting$Game_ID))
{
  newpt <- data.frame(month = partitionedTesting$Month[i], medianViewsPerMatchUp = partitionedTesting$Median_Views_Per_Matchup[i], bestRankAmongTeams = partitionedTesting$bestRankAmongTeams[i], gameType = partitionedTesting$gameType[i])
  partitionedTesting$modelOnePredictions[i] <- predict(model1, newdata = newpt)
  partitionedTesting$modelOneDeviation[i] <- abs((partitionedTesting$Tot_Viewers[i] - partitionedTesting$modelOnePredictions[i])/partitionedTesting$Tot_Viewers[i])
}

mean(partitionedTesting$modelOneDeviation)


#Answer the testing data 
for(i in 1:length(testData$Game_ID))
{
  newpt <- data.frame(month = testData$Month[i], medianViewsPerMatchUp = testData$Median_Views_Per_Matchup[i], bestRankAmongTeams = testData$bestRankAmongTeams[i], gameType = testData$gameType[i])
  testData$Total_Viewers[i] <- predict(model1, newdata = newpt)
}
  



  