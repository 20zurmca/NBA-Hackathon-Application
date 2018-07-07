library(sqldf)
setwd("ProjectRepos/NBAH18/Business\ Analytics")

#importing game data

gameData <- read.csv("game_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)



groupByQuery <- sqldf('select Game_ID, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

sqldf('select* from groupByQuery order by Tot_Viewers desc')
sqldf('select Home_Team, Away_Team from training where Game_ID = 21700015 and Game_Date = "10/19/2017"')

#average all CLE intl viewers 
#average intl viewers for all games
#compare


gamesCLE <- sqldf('select count(*) from training where Home_Team = "CLE" OR Away_Team = "CLE"')

sqldf('select * from gameData where Game_ID = "21700215"')

gamesCLE
