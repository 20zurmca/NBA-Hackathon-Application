eventCodes <- read.csv("Event_Codes.txt", header = T, sep= "\t")
gameLineupData <- read.csv("Game_Lineup_Data_Sample.txt", header = T, sep= "\t")
playByPlayData <- read.csv("Play_by_Play_Data_Sample.txt", header = T, sep = "\t")


#a player makes a shot if Event_Msg_Type = 1

madeShots <- playByPlayData[which(playByPlayData$Event_Msg_Type == 1),]

madeShots

playByPlayData[,"WC_Time"]
