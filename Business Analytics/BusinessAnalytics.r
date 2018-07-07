library(sqldf)
library(lubridate)
setwd("ProjectRepos/NBAH18/Business\ Analytics")

#importing game data

gameData <- read.csv("game_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)



groupByQuery <- sqldf('select Game_ID, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

#analyzing specific dates for total viewers
sqldf('select* from groupByQuery order by Tot_Viewers desc')

#specific game query
sqldf('select Home_Team, Away_Team from training where Game_ID = 21700015 and Game_Date = "10/19/2017"')


#Convert all dates to months of the year
groupByQuery$Month <- month(as.POSIXlt(groupByQuery$Game_Date, format = "%m/%d/%Y"))


#average all CLE intl viewers 
#average intl viewers for all games
#compare
gamesCLE <- sqldf('select count(*) from training where Home_Team = "CLE" OR Away_Team = "CLE"')



#determine if it's Christmas or opening day.  Set to C for Christmas, O for opening day, and R for any other game.
groupByQuery$gameType <- NULL
for(i in 1:length(groupByQuery$Game_Date))
{
  if(groupByQuery$Game_Date[i] == "12/25/2016" || groupByQuery$Game_Date[i] == "12/25/2017" || groupByQuery$Game_Date[i] == "12/25/2018")
  {
    groupByQuery$gameType[i] = "C"
  }
  
  else if(groupByQuery$Game_Date[i] == "10/25/2016" || groupByQuery$Game_Date[i] == "10/17/2017")
  {
    groupByQuery$gameType[i] = "O"
  }
  else
  {
    groupByQuery$gameType[i] = "R"
  }
}

groupByQuery$gameType <- as.factor(groupByQuery$gameType);

