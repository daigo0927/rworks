
years <- as.character(seq(2016, 2016, 1))

for(y in 1:length(years)){

	year <- years[y]
	NBApath <- paste('../Daigo/Desktop/NBA', year, sep='/')
	# ../NBA/20XX

	Num_teams <- 30



	# preprocess for regular season game result****************************
	regular_file <- paste(NBApath, 'regular.csv', sep='/')
	# ../NBA/20XX/regular.csv

	matches <- read.csv(regular_file, header=F)
	colnames(matches) <- c('date', 'team1', 'points1', 'team2', 'points2', 'state')
	matches$team1 <- as.character(matches$team1)
  	matches$team2 <- as.character(matches$team2)	
  	matches$state <- as.character(matches$state)
  	# "at Atlanta" -> "Atlanta"
  	matches$state <- sub('at ', '', matches$state)

  	# get teamname
  	teams <- sort(unique(matches$team1))
  	# Atlanta Hawks ,,,
  	
  	# numberize regular season game result
  	for(i in 1:nrow(matches)){

  		matches[i,]$team1 <- which(matches[i,]$team1 == teams)
  		matches[i,]$team2 <- which(matches[i,]$team2 == teams)

  		state_num = as.character(grep(matches[i,]$state, teams)[1])
  		if(matches[i,]$team1 == state_num || matches[i,]$team2 == state_num){
  			matches[i,]$state <- state_num
  		}else{matches[i,]$state <- '14'}

  	}
  	matches$team1 <- as.numeric(matches$team1)
  	matches$team2 <- as.numeric(matches$team2)
  	matches$state <- as.numeric(matches$state)




  	# read playoffs game result ************************************************
  	playoffs_file <- paste(NBApath, 'playoffs.csv', sep='/')
  	# ../Desktop/NBA/20XX/playoffs.csv

  	# preprocess
  	matches_playoffs <- read.csv(playoffs_file, header=F)
  	colnames(matches_playoffs) <- c('date', 'team1', 'points1', 'team2', 'points2', 'state')
	matches_playoffs$team1 <- as.character(matches_playoffs$team1)
  	matches_playoffs$team2 <- as.character(matches_playoffs$team2)	
  	matches-playoffs$state <- as.character(matches_playoffs$state)
  	# "at Atlanta" -> "Atlanta"
  	matches_playoffs$state <- sub('at ', '', matches_playoffs$state)

  	# numberize
  	for(i in 1:nrow(matches_playoffs)){
  		matches_playoffs[i,]$team1 <- which(matches_playoffs[i,]$team1 == teams)
  		matches_playoffs[i,]$team2 <- which(matches_playoffs[i,]$team2 == teams)

  		state_num = as.character(grep(matches_playoffs[i,]$state, teams)[1])
  		if(matches_playoffs[i,]$team1 == state_num || matches_playoffs[i,]$team2 == state_num){
  			matches_playoffs[i,]$state <- state_num
  		}else{matches_playoffs[i,]$state <- '14'}
  	}
  	matches_playoffs$team1 <- as.numeric(matches_playoffs$team1)
  	matches_playoffs$team2 <- as.numeric(matches_playoffs$team2)
  	matches_playoffs$state <- as.numeric(matches_playoffs$state)



  	# read stats data ********************************************************

	File_names <- paste(gsub(' ', '_', teams), 'csv', sep='.')
  	# Atlanta_Hawks.csv ,,,
  	stats_Files <- paste(NBApath, File_names, sep='/')
  	# path ../NBA/20XX/Atlanta_Hawks.csv ,,,

  	Stats_original <- list()
  	Stats <- list()

  	# preprocess stats data
  	for(t in 1:Num_teams){

  		if(file.access(stats_Files[t]) != 0){
  			Stats[[t]] <- matrix(NA, nrow=Num_teams, ncol=ncol(Stats[[t-1]]))
  			colnames(Stats[[t]]) <- colnames(Stats[[t-1]])
  			rownames(Stats[[t]]) <- teams
  			next
  		}

  		Stats_original[[t]] <- read.csv(stats_Files[t], header=T)
  		Stats_original[[t]]$v..Team <- sub('v. ', '', Stats_original[[t]]$v..Team)
  		Stats_original[[t]] <- Stats_original[[t]][,-22]

  		Stats[[t]] <- matrix(0, nrow=Num_teams, ncol=ncol(Stats_original[[t]]))

  		for(i in 1:nrow(Stats_original[[t]])){
  			Stats[[t]][grep(Stats_original[[t]][i,]$v..Team, teams), ] <- t(Stats_original[[t]][i,])
  		}

  		colnames(Stats[[t]]) <- colnames(Stats_original[[t]])
  		rownames(Stats[[t]]) <- teams

  		Stats[[t]] <- apply(Stats[[t]][,-1], c(1,2), as.numeric)

  	}
  	names(Stats) <- teams

  	stats_names <- colnames(Stats[[1]])


  	# make voting matrix for rating ************************************
  	Vote <- list()

  	for(i in 1:length(stats_names)){
  		Vote[[i]] <- matrix(0, ncol=Num_teams, nrow=Num_teams)
  		rownames(Vote[[i]]) <- teams
  		colnames(Vote[[i]]) <- teams

  		for(j in 1:Num_teams){
  			Vote[[i]][,j] <- Stats[[j]][,i]
  		}
  	}
  	names(Vote) <- stats_names















}