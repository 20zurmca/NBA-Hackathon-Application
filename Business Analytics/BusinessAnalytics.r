#Cam, run lines 106 - 112 and see what it does

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

gameData <- read.csv("game_data.csv", header = T)
playerData <- read.csv("player_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)
numberAllStarsPerTeam2016_2017 <- read.csv("number_all_stars_per_team_2016-2017.csv", header = T)
numberAllStarsPerTeam2015_2016 <- read.csv("number_all_stars_per_team_2015-2016.csv", header = T)
testing<-read.csv("test_set.csv", header = T)

#use rankings2015 from 2015-2016 season for month of October 10/2016 only
rankings2015_2016 <- read.csv("rankings2015_2016.csv", header = T)
rankings2016_2017 <- read.csv("rankings2016_2017.csv", header = T)
hasTopFivePlayer2015_2016 <- read.csv("teams_with_top_players_2015-2016.csv", header = T)
hasTopFivePlayer2016_2017 <- read.csv("teams_with_top_players_2016-2017.csv", header = T)
time_zones <- read.csv("time_zones.csv", header = T)

totalViewersPerGame <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

clevelandGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Tot_Viewers from totalViewersPerGame where Home_Team = "CLE" OR Away_Team = "CLE"')

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

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

  return(sqldf(paste0("select Tot_Viewers from totalViewersPerGame where Home_Team ='", home, "'and Away_Team = '", away, "'"))$Tot_Viewers)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#################################################################### FUNCTIONS AND APPENDED COLLUMNS #########################################################################

#MONTHS AND TYPES OF GAMES

#Convert all dates to months of the year
totalViewersPerGame$Month <- month(as.POSIXlt(totalViewersPerGame$Game_Date, format = "%m/%d/%Y"))

#Adding in the day of week
totalViewersPerGame$Day <- weekdays(as.Date(totalViewersPerGame$Game_Date, format="%m/%d/%Y"))



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
  
  lebron <- "LeBron James"
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    if((totalViewersPerGame$Home_Team[i] == "CLE" || totalViewersPerGame$Away_Team[i] == "CLE"))
    {
      gameId <- totalViewersPerGame$Game_ID[i]
      
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
  
  myMedianDict <- list()
  
  for(i in 1:length(totalViewersPerGame$Game_ID))
  {
    homeTeam <- totalViewersPerGame$Home_Team[i]
    awayTeam <- totalViewersPerGame$Away_Team[i]
    
    if(is.integer(myMedianDict[paste0(homeTeam," vs. ", awayTeam)]))
      {
        medianViewsForMatchup[i] <- myMedianDict[paste0(homeTeam," vs. ", awayTeam)]
        
      } else {
        
        medianView <- median(getHelperQuery(homeTeam, awayTeam))
        
        medianViewsForMatchup[i] <- medianView
        myMedianDict[paste0(homeTeam," vs. ", awayTeam)] <- medianView
      }
  }
  return(medianViewsForMatchup)
}

totalViewersPerGame["Median_Views_Per_Matchup"] <- computeMedianMatchup()


#Appending a Row "Is_Weekend": 'Yes' = saturday or sunday, 'No' = else. Recall sat/sunday were statistically significant for viewership

isWeekend <- function()
{
  isWeekend <- NULL
  for(i in 1:length(totalViewersPerGame$Day))
  {
    if(totalViewersPerGame$Day[i] == "Saturday" || totalViewersPerGame$Day[i] == "Sunday")
    {
      isWeekend[i] <- "YES"
    } else 
    {
      isWeekend[i] <- "NO"
    }
  }
  return(isWeekend)
}

totalViewersPerGame["Is_Sat_or_Sun"] <- isWeekend()

totalViewersPerGame$Is_Sat_or_Sun <- as.factor(totalViewersPerGame$Is_Sat_or_Sun)


#loads the best ranking team
loadBestRankingTeam <- function()
{
  j <- 1;
  while(j <= length(totalViewersPerGame$Tot_Viewers))
  {
    #if it's the month of October, use the highest ranking team from the previous season
    if(totalViewersPerGame$Month[j] == 10 && getYearFromDate(totalViewersPerGame$Game_Date[j]) == 16)
    {
      for(i in 1:length(rankings2015_2016$Rk))
      {
        if((totalViewersPerGame$Home_Team[j] == rankings2015_2016[i,3] || totalViewersPerGame$Away_Team[j] == rankings2015_2016[i,3]))
        {
          totalViewersPerGame$bestRankAmongTeams[j] <- i;
          break;
        }
      }
    } else if(totalViewersPerGame$Month[j] == 10 && getYearFromDate(totalViewersPerGame$Game_Date[j]) == 17)
    {
      for(i in 1:length(rankings2016_2017$Rk))
      {
        if((totalViewersPerGame$Home_Team[j] == rankings2016_2017[i,3] || totalViewersPerGame$Away_Team[j] == rankings2016_2017[i,3]))
        {
          totalViewersPerGame$bestRankAmongTeams[j] <- i;
          break;
        }
      }
    } else  
    {
      homeTeam <- totalViewersPerGame$Home_Team[j]
      awayTeam <- totalViewersPerGame$Away_Team[j]
      
      rankHome <- sqldf(paste0("select teamRanking from teamRankings where Game_Date = '", totalViewersPerGame$Game_Date[j], "' and Team = '", totalViewersPerGame$Home_Team[j], "'"))
      rankAway <- sqldf(paste0("select teamRanking from teamRankings where Game_Date = '", totalViewersPerGame$Game_Date[j], "' and Team = '", totalViewersPerGame$Away_Team[j], "'"))
      
      if(rankHome$teamRanking > rankAway$teamRanking)
      {
        totalViewersPerGame$bestRankAmongTeams[j] <- rankAway$teamRanking
      } else
      {
        totalViewersPerGame$bestRankAmongTeams[j] <- rankHome$teamRanking
      }
    }
    j <- j+1
  }
}

#################################################### MORE QUERIES ########################################33
viewsByTime_Zone <- sqldf('select Time_Zone_Of_Game, sum("Tot_Viewers") as Tot_Viewers from totalViewersPerGame group by Time_Zone_of_Game')

eastGames     <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Time_Zone_Of_Game, Tot_Viewers from totalViewersPerGame where Time_Zone_Of_Game = "EAST"')
centralGames  <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Time_Zone_Of_Game, Tot_Viewers from totalViewersPerGame where Time_Zone_Of_Game = "CENTRAL"')
mountainGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Time_Zone_Of_Game, Tot_Viewers from totalViewersPerGame where Time_Zone_Of_Game = "MOUNTAIN"')
pacificGames  <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Time_Zone_Of_Game, Tot_Viewers from totalViewersPerGame where Time_Zone_Of_Game = "PACIFIC"')

topPlayerGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Has_Top_Five_Player, Tot_Viewers from totalViewersPerGame where Has_Top_Five_Player = "YES"')
noTopPlayerGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Has_Top_Five_Player, Tot_Viewers from totalViewersPerGame where Has_Top_Five_Player = "NO"')

allStarGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, All_Star_Count, Tot_Viewers from totalViewersPerGame where All_Star_Count > 0')
noAllStars   <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, All_Star_Count, Tot_Viewers from totalViewersPerGame where All_Star_Count = 0')

groupByAllStarCount <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, All_Star_Count, sum(Tot_Viewers) as Tot_Viewers from totalViewersPerGame where All_Star_Count > 0 group by All_Star_Count')

#Calculating average views per all star count in game
Average_Views_Per_All_Star_Count <- NULL
Number_of_Games <- NULL
for(i in 1:length(groupByAllStarCount$All_Star_Count))
{
  all_star_query <- sqldf(paste0("select * from totalViewersPerGame where All_Star_Count =" ,i))
  Average_Views_Per_All_Star_Count[i] <- sum(all_star_query$Tot_Viewers)/length(all_star_query$Game_ID)
  Number_of_Games[i] <- length(all_star_query$Game_ID)
}

groupByAllStarCount$Average_Views_Per_All_Star_Count <- Average_Views_Per_All_Star_Count
groupByAllStarCount$Number_of_Games <- Number_of_Games #there seems to be an upward trend

gamesWithLebron <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Is_Lebron_Playing, Tot_Viewers from totalViewersPerGame where Is_Lebron_Playing = "YES"')
gamesWithoutLebron <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Is_Lebron_Playing, Tot_Viewers from totalViewersPerGame where Is_Lebron_Playing = "NO"')

weekendGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Day, Tot_Viewers from totalViewersPerGame where Day = "Saturday" or Day= "Sunday"')
weekdayGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Day, Tot_Viewers from totalViewersPerGame where NOT Day = "Saturday" and NOT Day= "Sunday"')

octoberGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Month, Tot_Viewers from totalViewersPerGame where Month = 10')

######################################## MORE HYPOTHESIS TESTS ###############################################################

#testing if weekendGames have significant effect
z.test(mean(weekendGames$Tot_Viewers), meanNumberTotalViewers, sd(weekendGames$Tot_Viewers), length(weekendGames$Tot_Viewers), 0.95, "greater")
#Statistically significant results at the 5% level if Days are Sunday or Saturday (more viewers) if Friday is included, results are NOT significant at the .10 level

z.test(mean(weekdayGames$Tot_Viewers), meanNumberTotalViewers, sd(weekdayGames$Tot_Viewers), length(weekdayGames$Tot_Viewers), 0.95, "less")
#Not significant that weekday games have LESS viewers as the 10% level

#testing if top five players have a significant effect
z.test(mean(topPlayerGames$Tot_Viewers), meanNumberTotalViewers, sd(topPlayerGames$Tot_Viewers), length(topPlayerGames$Tot_Viewers), 0.95, "greater")
z.test(mean(noTopPlayerGames$Tot_Viewers), meanNumberTotalViewers, sd(noTopPlayerGames$Tot_Viewers), length(topPlayerGames$Tot_Viewers), 0.95, "less")
#We have highly statistically significant results (p <<.01) that top players in the game drive viewership (not as much as LeBron alone, however)

#Testing if all stars have an effect
z.test(mean(allStarGames$Tot_Viewers), meanNumberTotalViewers, sd(allStarGames$Tot_Viewers), length(allStarGames$Tot_Viewers), 0.95, "greater")
#Significant results (p < .01)
z.test(mean(noAllStars$Tot_Viewers), meanNumberTotalViewers, sd(noAllStars$Tot_Viewers), length(noAllStars$Tot_Viewers), 0.95, "less")
#Significant results (p < .01)

#Testing if Lebron playing has an effect
mean(gamesWithLebron$Tot_Viewers) #Games with lebron have over 2x the amount of average viewers
mean(gamesWithoutLebron$Tot_Viewers)

z.test(mean(gamesWithLebron$Tot_Viewers), meanNumberTotalViewers, sd(gamesWithLebron$Tot_Viewers), length(gamesWithLebron$Tot_Viewers), 0.95, "greater")
#The significance of this data is sky high
z.test(mean(gamesWithoutLebron$Tot_Viewers), meanNumberTotalViewers, sd(gamesWithoutLebron$Tot_Viewers), length(gamesWithoutLebron$Tot_Viewers), 0.95, "less")
# P-val is <0.01 that games without lebron are watched less on average

mean(eastGames$Tot_Viewers)
mean(mountainGames$Tot_Viewers)
mean(pacificGames$Tot_Viewers)
mean(centralGames$Tot_Viewers)

boxplot(totalViewersPerGame$Tot_Viewers ~ totalViewersPerGame$Time_Zone_Of_Game, main = "Int'l Viewership by TimeZone")

#Test by timezone

#East
z.test(mean(eastGames$Tot_Viewers), meanNumberTotalViewers, sd(eastGames$Tot_Viewers), length(eastGames$Tot_Viewers), 0.95, "greater")
#Not significant at 10% level

#Central
z.test(mean(centralGames$Tot_Viewers), meanNumberTotalViewers, sd(centralGames$Tot_Viewers), length(centralGames$Tot_Viewers), 0.95, "less")
#Not significant at the 10% level

#Pacific
z.test(mean(pacificGames$Tot_Viewers), meanNumberTotalViewers, sd(pacificGames$Tot_Viewers), length(pacificGames$Tot_Viewers), 0.95, "greater")
#Statistically significant (P << 0.01)

#Mountain
z.test(mean(mountainGames$Tot_Viewers), meanNumberTotalViewers, sd(mountainGames$Tot_Viewers), length(mountainGames$Tot_Viewers), 0.95, "less")
#Statistically significant (P << .01)

mean(octoberGames$Tot_Viewers)
z.test(mean(octoberGames$Tot_Viewers), meanNumberTotalViewers, sd(octoberGames$Tot_Viewers), length(octoberGames$Tot_Viewers), 0.95, "greater")
#Yes October is statistically significant at the 1% level (p << .01)


#Testing averages depending on best ranked team in the matchup

rankViewAverages <- sqldf('select bestRankAmongTeams, avg(Tot_Viewers) as Avg_Viewers from totalViewersPerGame group by bestRankAmongTeams')
plot(rankViewAverages$Avg_Viewers~rankViewAverages$bestRankAmongTeams)
myTable <- sqldf('select Tot_Viewers from totalViewersPerGame where bestRankAmongTeams = 1')

z.test(mean(myTable$Tot_Viewers), meanNumberTotalViewers, sd(myTable$Tot_Viewers), length(myTable$Tot_Viewers), 0.95, "greater")

#ranks

#--------------------------------------------------------------------------------------------------------------------------------#

#Ranking teams partitioned by game date
teamRankings <- data.table(gameData, key = "Game_Date")
teamRankings <- teamRankings[,transform(.SD, teamRanking = rank(-Wins_Entering_Gm, ties.method = "min")), by = Game_Date]
teamRankings <- teamRankings[,c("Game_Date", "Team", "Wins_Entering_Gm", "teamRanking")]


#use rankings2015 from 2015-2016 season for month of October 10/2016 only


totalViewersPerGame$bestRankAmongTeams <- NULL;

#function to get best ranking team

loadBestRankingTeam()

