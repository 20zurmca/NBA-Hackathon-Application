library(sqldf)

#Before pushing, comment out my working directory and uncomment yours

rm(list = ls())

#setwd("ProjectRepos/NBAH18/Business\ Analytics")
setwd("/home/cameron/NBAH18/Business\ Analytics")

#importing data

gameData <- read.csv("game_data.csv", header = T)
playerData <- read.csv("player_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)



totalViewersPerGame <- sqldf('select Game_ID, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

#Seeing what dates/games were most popular. We noticed that Christmas/opening day had a large effect, as well as the caliber of the teams playing
sqldf('select* from totalViewersPerGame order by Tot_Viewers desc') 

#average all CLE intl viewers 
#average intl viewers for all games
#compare


