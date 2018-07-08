#Cam, run lines 106 - 112 and see what it does

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

gameData <- read.csv("game_data.csv", header = T)
playerData <- read.csv("player_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)
numberAllStarsPerTeam2016_2017 <- read.csv("number_all_stars_per_team_2016-2017.csv", header = T)
numberAllStarsPerTeam2015_2016 <- read.csv("number_all_stars_per_team_2015-2016.csv", header = T)
testing<-read.csv("test_set.csv", header = T)

#use rankings2015 from 2015-2016 season for month of October 10/2016 only
rankings2015_2016 <- read.csv("rankings2015_2016.csv", header = T)
hasTopFivePlayer2015_2016 <- read.csv("teams_with_top_players_2015-2016.csv", header = T)
hasTopFivePlayer2016_2017 <- read.csv("teams_with_top_players_2016-2017.csv", header = T)
time_zones <- read.csv("time_zones.csv", header = T)

totalViewersPerGame <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

clevelandGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Tot_Viewers from totalViewersPerGame where Home_Team = "CLE" OR Away_Team = "CLE"')

lebronPlayerData <- sqldf('select Season, Game_ID, Game_Date, Name, Active_Status from playerData where Name = "LeBron James"')

lebronActiveGames <- sqldf('select * from lebronPlayerData INNER JOIN clevelandGames on clevelandGames.Game_Date = lebronPlayerData.Game_Date where Active_Status = "Active"')
lebronActiveGames <- lebronActiveGames[ , -c(6,9)] #cleaning duplicates
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

###################################################### CHARTS #################################################################################################

#Looking at the distributing of totalViewersPerGame-We see that is is heavily skewed right
qqnorm(totalViewersPerGame$Tot_Viewers, main = "Normal Q-Q Plot for the total viewers")
qqline(totalViewersPerGame$Tot_Viewers)
summary(totalViewersPerGame$Tot_Viewers)
meanNumberTotalViewers <- mean(totalViewersPerGame$Tot_Viewers)
sdTotalViewers <- sd(totalViewersPerGame$Tot_Viewers)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


######################################## Z TEST FOR CLE'S MEAN VIEWERSHIP #####################################################################################################
#Performing a z-test to see if CLE's average is higher than the overall. First getting only Cleveland games

#We notice that the clevelandGames$Tot_Viewers still has a right skew, but since n is so large (129), CLT applies
qqnorm(clevelandGames$Tot_Viewers, main = "Normal Q-Q Plot for Cleveland's Viewership")
qqline(clevelandGames$Tot_Viewers)

#Gathering statistics
sum(clevelandGames$Tot_Viewers)
clevelandMeanViews <- mean(clevelandGames$Tot_Viewers)
clevelandN <- length(clevelandGames$Tot_Viewers)
clevelandSd <- sd(clevelandGames$Tot_Viewers)


z.test(clevelandMeanViews, meanNumberTotalViewers, clevelandSd, clevelandN, 0.95, "greater")

#Z statistic is so large (15)..this is compelling evidence that people watch Lebron

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

####################################################################### QUERIES ############################################################################################
#Seeing what dates/games were most popular. We noticed that Christmas/opening day had a large effect, as well as the caliber of the teams playing
sqldf('select* from totalViewersPerGame order by Tot_Viewers desc')

#analyzing specific dates for total viewers
sqldf('select* from totalViewersPerGame order by Tot_Viewers desc')

#specific game query
sqldf('select Home_Team, Away_Team from training where Game_ID = 21700015 and Game_Date = "10/19/2017"')

#Experimenting getting number of all stars for a team from numberAllStarsPerTeam2015_2016
sqldf('select Number_of_All_Stars from numberAllStarsPerTeam2015_2016 where Team = "CLE"')$Number_of_All_Stars
numberAllStarsPerTeam2015_2016[numberAllStarsPerTeam2015_2016$Team == "CLE",]$Number_of_All_Stars

sqldf('select * from totalViewersPerGame where Home_Team = "CLE" and Away_Team = "PHI" or Home_Team = "PHI" and Away_Team = "CLE"')

sqldf('select * from totalViewersPerGame where Home_Team = "CLE" and Away_Team = "GSW" or Home_Team = "GSW" and Away_Team = "CLE"')


#-------------------------------------------------------------------------------------------------------------------------------------------------------

############################################################ HELPER FUNCTIONS ##########################################################################

getYearFromDate <- function(date)
{
  return(substr(format(as.Date(date, format="%m/%d/%Y","%Y")), 3, 4))
}


lookUpAllStarCountFor <- function(team, month, year)
{
  if(year == "16" || (year == "17" && month < 10)) #results based on 2015-2016 season
  {
    return(numberAllStarsPerTeam2015_2016[numberAllStarsPerTeam2015_2016$Team == team,]$Number_of_All_Stars)
    
  } else
  {
    return(numberAllStarsPerTeam2016_2017[numberAllStarsPerTeam2016_2017$Team == team,]$Number_of_All_Stars)
  }
}

lookUpTopPlayerStatusFor <- function(team, month, year) 
{
  if(year == "16" || (year == "17" && month < 10))
  {
    return(hasTopFivePlayer2015_2016[hasTopFivePlayer2015_2016$Team == team,]$Has_Top_Five_Player)
    
  } else
  {
    return(hasTopFivePlayer2016_2017[hasTopFivePlayer2016_2017$Team == team,]$Has_Top_Five_Player)
  }
}

getHelperQuery <- function(home, away)
{

  return(sqldf(paste0("select * from totalViewersPerGame where Home_Team ='", home, "'and Away_Team = '", away, "' or Home_Team = '", away, "' and Away_Team = '", home, "'"))$Tot_Viewers)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################### FUNCTIONS AND APPENDED COLLUMNS #########################################################################

#MONTHS AND TYPES OF GAMES

#Convert all dates to months of the year
totalViewersPerGame$Month <- month(as.POSIXlt(totalViewersPerGame$Game_Date, format = "%m/%d/%Y"))

#Comparative Boxplot for viewership and months
boxplot(totalViewersPerGame$Tot_Viewers ~ totalViewersPerGame$Month, main = "Comparative Boxplots for Viewers Per Month", xlab = "Month of Year", ylab = "# of Int'l Viewers")

#Adding a collumn for GameType
# First determine if it's Christmas or opening day.  Set to C for Christmas, O for opening day, and R for any other game.
totalViewersPerGame$gameType <- NULL
for(i in 1:length(totalViewersPerGame$Game_Date))
{
  if(totalViewersPerGame$Game_Date[i] == "12/25/2016" || totalViewersPerGame$Game_Date[i] == "12/25/2017")
  {
    totalViewersPerGame$gameType[i] = "C"
  }
  
  else if(totalViewersPerGame$Game_Date[i] == "10/25/2016" || totalViewersPerGame$Game_Date[i] == "10/17/2017")
  {
    totalViewersPerGame$gameType[i] = "O"
  }
  else
  {
    totalViewersPerGame$gameType[i] = "R"
  }
}

totalViewersPerGame$gameType <- as.factor(totalViewersPerGame$gameType);



#A Function that will calculate how many All-Stars are playing at the game.
getAllStarCount <- function()
{
  All_Star_Count <- NULL
  for(i in 1:length(totalViewersPerGame$Game_Date))
  {
     year <- getYearFromDate(totalViewersPerGame$Game_Date[i])
     homeAllStarCount <- lookUpAllStarCountFor(totalViewersPerGame$Home_Team[i], totalViewersPerGame$Month[i], year)
     awayAllStarCount <- lookUpAllStarCountFor(totalViewersPerGame$Away_Team[i], totalViewersPerGame$Month[i], year)
     All_Star_Count[i] = (homeAllStarCount + awayAllStarCount)
  }
  return(All_Star_Count)
}

totalViewersPerGame["All_Star_Count"] <- getAllStarCount()


#A Function that determines if Lebron is in the game
hasLebron <- function()
{
  Has_Lebron <- NULL
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    if(totalViewersPerGame$Game_ID[i] == 21601218)
    {
      Has_Lebron[i] = "NO"
    }
    else if((totalViewersPerGame$Home_Team[i] == "CLE" || totalViewersPerGame$Away_Team[i] == "CLE"))
    {
      Has_Lebron[i] = "YES"
    } else {
      Has_Lebron[i] = "NO"
    }
  }
  return (Has_Lebron)
}

totalViewersPerGame["Is_Lebron_Playing"] <- hasLebron()

totalViewersPerGame$Is_Lebron_Playing<- as.factor(totalViewersPerGame$Is_Lebron_Playing)

#A Function that determines if a top 5 player is on the team 
hasTopFivePlayer <- function()
{
  Has_Top_Five <- NULL
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    year <- getYearFromDate(totalViewersPerGame$Game_Date[i])
    homeTeamStatus <- lookUpTopPlayerStatusFor(totalViewersPerGame$Home_Team[i], totalViewersPerGame$Month[i], year)
    awayTeamStatus <- lookUpTopPlayerStatusFor(totalViewersPerGame$Away_Team[i], totalViewersPerGame$Month[i], year)
    
    if(homeTeamStatus == "YES" || awayTeamStatus == "YES")
    {
      Has_Top_Five[i] = "YES"
    } else 
    {
      Has_Top_Five[i] = "NO"
    }
  }
  return(Has_Top_Five)
}

totalViewersPerGame["Has_Top_Five_Player"] <- hasTopFivePlayer()

totalViewersPerGame$Has_Top_Five_Player <- as.factor(totalViewersPerGame$Has_Top_Five_Player)

#A Function that loads in the time zones for the games

loadTimeZones <- function()
{
  timezones <- NULL
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    zone <- time_zones[time_zones$Team == totalViewersPerGame$Home_Team[i], ]$Time_Zone
    if(zone == "EAST")
    {
      timezones[i] <- "EAST"
    } else if(zone == "CENTRAL")
    {
      timezones[i] <- "CENTRAL"
    } else if(zone == "PACIFIC")
    {
      timezones[i] <- "PACIFIC"
    } else {
      timezones[i] <- "MOUNTAIN"
    }
  }
  return(timezones)
}

totalViewersPerGame["Time_Zone_Of_Game"] <- loadTimeZones()
totalViewersPerGame$Time_Zone_Of_Game <- as.factor(totalViewersPerGame$Time_Zone_Of_Game)

# A function that will compute the medians number of viewers for the match-up

computeMedianMatchup<- function()
{
  medianViewsForMatchup <- NULL
  
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    
    medianViewsForMatchup[i] <- median(getHelperQuery(totalViewersPerGame$Home_Team[i], totalViewersPerGame$Away_Team[i]))
    
  }
  return(medianViewsForMatchup)
}

totalViewersPerGame["Median_Views_Per_Matchup"] <- computeMedianMatchup()


# A function that will compute the mean number of viewers for the match-up

computeMeanMatchup<- function()
{
  meanViewsForMatchup <- NULL
  
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    
    meanViewsForMatchup[i] <- mean(getHelperQuery(totalViewersPerGame$Home_Team[i], totalViewersPerGame$Away_Team[i]))
    
  }
  return(meanViewsForMatchup)
}

totalViewersPerGame["Mean_Views_Per_Matchup"] <- computeMeanMatchup()

#ranks

#--------------------------------------------------------------------------------------------------------------------------------#

#Ranking teams partitioned by game date
teamRankings <- data.table(gameData, key = "Game_Date")
teamRankings <- teamRankings[,transform(.SD, teamRanking = rank(-Wins_Entering_Gm, ties.method = "min")), by = Game_Date]
teamRankings <- teamRankings[,c("Game_Date", "Team", "Wins_Entering_Gm", "teamRanking")]
teamRankings

#gameData[91:length(gameData$Game_Date),c(3,4,6)]


#use rankings2015 from 2015-2016 season for month of October 10/2016 only


totalViewersPerGame$bestRankAmongTeams <- NULL;

j <- 1;
while(j < length(totalViewersPerGame$Tot_Viewers))
{
  #if it's the month of October, use the highest ranking team from the previous season
  if(totalViewersPerGame$Month[j] == 10)
  {
    for(i in 1:length(rankings2015_2016$Rk))
    {
      
      if((totalViewersPerGame$Home_Team[j] == rankings2015_2016[i,3] || totalViewersPerGame$Away_Team[j] == rankings2015_2016[i,3]))
      {
        totalViewersPerGame$bestRankAmongTeams[j] <- i;
        break;
      }
      
    }
  }

  else
  {
    
    homeTeam <- totalViewersPerGame$Home_Team[j]
    awayTeam <- totalViewersPerGame$Away_Team[j]
    
    rankHome <- gameData[(which("Game_Date" == totalViewersPerGame$Game_Date[j])) && (which("Team" == homeTeam)),"teamRanking"]
    rankAway <- gameData[(which("Game_Date" == totalViewersPerGame$Game_Date[j])) && (which("Team" == awayTeam)),"teamRanking"]
    
   # print(rankHome)
    
    
    
    if(rankHome > rankAway)
    {
      totalViewersPerGame$bestRankAmongTeams[j] <- rankAway
    }
    
    else
    {
      totalViewersPerGame$bestRankAmongTeams[j] <- rankHome
    }
  }
  j <- j+1
}

totalViewersPerGame$gameType <- as.factor(totalViewersPerGame$gameType)



