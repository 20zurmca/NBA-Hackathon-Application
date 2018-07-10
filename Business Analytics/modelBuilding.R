library(sqldf)
library(lubridate)
library(stringi)
library(data.table)

getwd()
#setwd("ProjectRepos/NBAH18/Business\ Analytics")
#setwd("/home/cameron/NBAH18/Business\ Analytics")
source("functions.R")

#Before pushing, comment out my working directory and uncomment yours

#setwd("ProjectRepos/NBAH18/Business\ Analytics")

########################################################## IMPORTING DATA ####################################################################################################
testing<-read.csv("test_set.csv", header = T)
gameData <- read.csv("game_data.csv", header = T)
playerData <- read.csv("player_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)
totalViewersPerGame <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

numberAllStarsPerTeam2016_2017 <- read.csv("number_all_stars_per_team_2016-2017.csv", header = T)
numberAllStarsPerTeam2015_2016 <- read.csv("number_all_stars_per_team_2015-2016.csv", header = T)
hasTopFivePlayer2015_2016 <- read.csv("teams_with_top_players_2015-2016.csv", header = T)
hasTopFivePlayer2016_2017 <- read.csv("teams_with_top_players_2016-2017.csv", header = T)
time_zones <- read.csv("time_zones.csv", header = T)

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
  
  return(sqldf(paste0("select Tot_Viewers from totalViewersPerGame where Home_Team ='", home, "'and Away_Team = '", away, "'"))$Tot_Viewers)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################### FUNCTIONS AND APPENDED COLLUMNS #########################################################################

#MONTHS AND TYPES OF GAMES

#Convert all dates to months of the year
testing$Month <- month(as.POSIXlt(testing$Game_Date, format = "%m/%d/%Y"))

#Adding in the day of week
testing$Day <- weekdays(as.Date(testing$Game_Date, format="%m/%d/%Y"))


#Adding a collumn for GameType
# First determine if it's Christmas or opening day.  Set to C for Christmas, O for opening day, and R for any other game.
testing$gameType <- NULL
for(i in 1:length(testing$Game_Date))
{
  if(testing$Game_Date[i] == "12/25/2016" || testing$Game_Date[i] == "12/25/2017")
  {
    testing$gameType[i] = "C"
  }
  
  else if(testing$Game_Date[i] == "10/25/2016" || testing$Game_Date[i] == "10/17/2017")
  {
    testing$gameType[i] = "O"
  }
  else
  {
    testing$gameType[i] = "R"
  }
}

testing$gameType <- as.factor(testing$gameType);



#A Function that will calculate how many All-Stars are playing at the game.
getAllStarCount <- function()
{
  All_Star_Count <- NULL
  for(i in 1:length(testing$Game_Date))
  {
    year <- getYearFromDate(totalViewersPerGame$Game_Date[i])
    homeAllStarCount <- lookUpAllStarCountFor(testing$Home_Team[i], testing$Month[i], year)
    awayAllStarCount <- lookUpAllStarCountFor(testing$Away_Team[i], testing$Month[i], year)
    All_Star_Count[i] = (homeAllStarCount + awayAllStarCount)
  }
  return(All_Star_Count)
}

testing["All_Star_Count"] <- getAllStarCount()


#A Function that determines if Lebron is in the game
hasLebron <- function()
{
  Has_Lebron <- NULL
  
  lebron <- "LeBron James"
  for(i in 1:length(testing$Game_ID))
  {
    if((testing$Home_Team[i] == "CLE" || testing$Away_Team[i] == "CLE"))
    {
      gameId <- testing$Game_ID[i]
      
      lebronGame <- sqldf(paste0("select * from playerData where Game_ID = '", gameId, "' and Name = '", lebron , "'"))
      
      activeStatus <- lebronGame$Active_Status
      
      if(activeStatus == "Active")
      {
        Has_Lebron[i] = "YES"
        next
      } else {
        Has_Lebron[i] = "NO"
        next
      }
    } else 
    {
      Has_Lebron[i] = "NO"
    }
  }
  return (Has_Lebron)
}

testing["Is_Lebron_Playing"] <- hasLebron()

testing$Is_Lebron_Playing<- as.factor(testing$Is_Lebron_Playing)

#A Function that determines if a top 5 player is on the team 
hasTopFivePlayer <- function()
{
  Has_Top_Five <- NULL
  for(i in 1:length(testing$Game_ID))
  {
    year <- getYearFromDate(testing$Game_Date[i])
    homeTeamStatus <- lookUpTopPlayerStatusFor(testing$Home_Team[i], testing$Month[i], year)
    awayTeamStatus <- lookUpTopPlayerStatusFor(testing$Away_Team[i], testing$Month[i], year)
    
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

testing["Has_Top_Five_Player"] <- hasTopFivePlayer()

testing$Has_Top_Five_Player <- as.factor(testing$Has_Top_Five_Player)

#A Function that loads in the time zones for the games

loadTimeZones <- function()
{
  timezones <- NULL
  for(i in 1:length(testing$Game_ID))
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

testing["Time_Zone_Of_Game"] <- loadTimeZones()
testing$Time_Zone_Of_Game <- as.factor(testing$Time_Zone_Of_Game)

# A function that will compute the medians number of viewers for the match-up

computeMedianMatchup<- function()
{
  medianViewsForMatchup <- NULL
  
  myMedianDict <- list()
  
  for(i in 1:length(testing$Game_ID))
  {
    homeTeam <- testing$Home_Team[i]
    awayTeam <- testing$Away_Team[i]
    
    if(is.integer(myMedianDict[paste0(homeTeam," vs. ", awayTeam)]))
    {
      medianViewsForMatchup[i] <- myMedianDict[paste0(homeTeam," vs. ", awayTeam)]
      
    } else {
      
      medianView <- median(getHelperQuery(homeTeam, awayTeam))
      
      if(is.na(medianView))
      {
        #Taking average viewership of both teams
        homeTeamViewership = median(sqldf(paste0("select Tot_Viewers from totalViewersPerGame where Home_Team = '", homeTeam, "'"))$Tot_Viewers)
        awayTeamViewership = median(sqldf(paste0("select Tot_Viewers from totalViewersPerGame where Away_Team = '", awayTeam, "'"))$Tot_Viewers)
        medianView <- median(c(homeTeamViewership, awayTeamViewership))
      }
      medianViewsForMatchup[i] <- medianView
      
      myMedianDict[paste0(homeTeam," vs. ", awayTeam)] <- medianView
    }
  }
  
  return(medianViewsForMatchup)
}

testing["Median_Views_Per_Matchup"] <- computeMedianMatchup()


#Appending a Row "Is_Weekend": 'Yes' = saturday or sunday, 'No' = else. Recall sat/sunday were statistically significant for viewership

isWeekend <- function()
{
  isWeekend <- NULL
  for(i in 1:length(testing$Day))
  {
    if(testing$Day[i] == "Saturday" || testing$Day[i] == "Sunday")
    {
      isWeekend[i] <- "YES"
    } else 
    {
      isWeekend[i] <- "NO"
    }
  }
  return(isWeekend)
}

testing["Is_Sat_or_Sun"] <- isWeekend()

testing$Is_Sat_or_Sun <- as.factor(testing$Is_Sat_or_Sun)


