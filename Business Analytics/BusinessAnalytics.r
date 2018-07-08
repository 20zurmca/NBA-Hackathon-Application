library(sqldf)

#Before pushing, comment out my working directory and uncomment yours

#setwd("ProjectRepos/NBAH18/Business\ Analytics")
setwd("/home/cameron/NBAH18/Business\ Analytics")

#importing data

gameData <- read.csv("game_data.csv", header = T)
playerData <- read.csv("player_data.csv", header = T)
training<-read.csv("training_set.csv", header = T)
hasTopFivePlayer <- read.csv("teams_with_top_players.csv", header = T)

totalViewersPerGame <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, sum("Rounded.Viewers") as Tot_Viewers from training group by Game_ID')

#Looking at the distributing of totalViewersPerGame-We see that is is heavily skewed right
qqnorm(totalViewersPerGame$Tot_Viewers, main = "Normal Q-Q Plot for the total viewers")
qqline(totalViewersPerGame$Tot_Viewers)
summary(totalViewersPerGame$Tot_Viewers)
meanNumberTotalViewers <- mean(totalViewersPerGame$Tot_Viewers)
sdTotalViewers <- sd(totalViewersPerGame$Tot_Viewers)

#Seeing what dates/games were most popular. We noticed that Christmas/opening day had a large effect, as well as the caliber of the teams playing
#sqldf('select* from totalViewersPerGame order by Tot_Viewers desc') 

#Performing a z-test to see if CLE's average is higher than the overall. First getting only Cleveland games
clevelandGames <- sqldf('select Game_ID, Home_Team, Away_Team, Game_Date, Tot_Viewers from totalViewersPerGame where Home_Team = "CLE" OR Away_Team = "CLE"')

#We notice that the clevelandGames$Tot_Viewers still has a right skew, but since n is so large (129), CLT applies
qqnorm(clevelandGames$Tot_Viewers, main = "Normal Q-Q Plot for Cleveland's Viewership")
qqline(clevelandGames$Tot_Viewers)

#Gathering statistics
sum(clevelandGames$Tot_Viewers)
clevelandMeanViews <- mean(clevelandGames$Tot_Viewers)
clevelandN <- length(clevelandGames$Tot_Viewers)
clevelandSd <- sd(clevelandGames$Tot_Viewers)

zStarCleveland <- qnorm(.95, 0 ,1) #Obtaining critical value to compare to
zStatCleveland <- (clevelandMeanViews - meanNumberTotalViewers)/(clevelandSd/sqrt(clevelandN)) 
#Z statistic is so large (15)..this is compelling evidence that people watch Lebron








