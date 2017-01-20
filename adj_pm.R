################################################################## 
### Calculating the Adjusted Plus/Minus                        ###
# Author: Konstantinos Pelechrinis                               #
# Date: 01/20/2017                                               #
# Input files: matchups20072008reg20081211.txt and		 #
# playerstats20072008reg20081211.txt from			 #
# http://basketballvalue.com/downloads.php			 #
################################################################## 

matchups <- read.csv("matchups20072008reg20081211.txt",sep="\t")
matchups$margin <- rep(0,dim(matchups)[1])
home.avg <- sum(matchups$PointsScoredHome)/sum(matchups$PossessionsHome)
away.avg <- sum(matchups$PointsScoredAway)/sum(matchups$PossessionsAway)


# calculate the per possession plus/minus
# if there are no possessions during a stint we will remove this data point later 
# if either the home or the away team does not have possession during the stint we use the average points per possession 
for (i in 1:dim(matchups)[1]){
 if (matchups$PossessionsHome[i] == 0 & matchups$PossessionsAway[i] == 0){
	matchups$margin[i] = NA
	next
  }
  if (matchups$PossessionsHome[i] > 0){
	hv = matchups$PointsScoredHome[i]/(matchups$PossessionsHome[i])
  } else {
	hv = home.avg
  }
  if (matchups$PossessionsAway[i] > 0) {
	av = matchups$PointsScoredAway[i]/(matchups$PossessionsAway[i])
  } else {
	av = away.avg
  }
  matchups$margin[i] = 100*(hv-av)
}

#this is the threshold for the minimum number of minutes played for a player in order to be included in the regression
#we use 388 that is used from basketball reference for comparisson reasons
min_thres <- 388

players.dat <- read.csv("playerstats20072008reg20081211.txt",sep="\t")
ind <- which(players.dat$SimpleMin>388)
players.dat$PID <- rep("",dim(players.dat)[1])

for (i in 1:dim(players.dat)[1]){
	players.dat$PID[i] <- paste0("P",as.character(players.dat$PlayerID[i]))
}

# add columns for the players that will be included in the regression
names <- players.dat$PID
matchups[,names[ind]] <- 0


# if a player was playing for the home team is labeled with "1", if he was playing for the visiting team he is labeled with -1
for (i in 1:dim(matchups)[1]){
	for (j in 5:9){
		tmp = paste0("P",as.character(matchups[i,j]))
		matchups[i,which(colnames(matchups)==tmp)] = 1
	}
	for (j in 10:14){
		tmp = paste0("P",as.character(matchups[i,j]))
                matchups[i,which(colnames(matchups)==tmp)] = -1
	}
}

matchups$Poss = matchups$PossessionsHome + matchups$PossessionsAway

ind_nna <- which(is.na(matchups$margin)==FALSE) 

adj0708 <- matchups[ind_nna,]

f <- paste("margin ~ ", paste(names[ind], collapse=" + "))

adjpm.mod <- lm(f, data=adj0708)

