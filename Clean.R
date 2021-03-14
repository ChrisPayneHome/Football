library(stringr)
library(readr)
library(dplyr)
library(rjson)

path <- "/Users/christian/Kaggle/Sports Analytics/Football/"
years <- c('21', '20', '19', '18', '17', '16', '15', '14', '13', '12', '11', '10', '09', '08', '07', '06', '05', '04', '03', '02', '01', '00')
game_files <- list.files(path = paste(path, "Game Data", sep = ""))

for(i in 22:2){
	game_name <- paste(years[i], years[i-1], ".csv", sep = "")
	data <- read.csv(paste(path, "Prem Game Data/", game_name, sep = ""), stringsAsFactors = FALSE)
	Team <- unique(data$HomeTeam)
	season_data <- data.frame(Team)
	season_data$Team <- as.character(season_data$Team)
	season_data$Pos <- 0
	season_data$Points <- 0
	season_data$Games <- 0
	season_data$Wins <- 0
	season_data$Draws <- 0
	season_data$Losses <- 0
	season_data$Win.Rate <- 0
	season_data$YC <- 0
	season_data$RC <- 0
	season_data$Fouls <- 0
	season_data$Corners <- 0
	season_data$Goals <- 0
	season_data$Conceeded <- 0
	season_data$GD <- 0
	season_data$Shots <- 0
	season_data$SonT <- 0
	season_data$Shot.Accuracy <- 0
	for(n in 1:nrow(season_data)){
		for(x in nrow(data):1){
			if(identical(data$HomeTeam[x], season_data$Team[n])){
				season_data$Goals[n] <- season_data$Goals[n] + data$FTHG[x]
				season_data$Conceeded[n] <- season_data$Conceeded[n] + data$FTAG[x]
				season_data$YC[n] <- season_data$YC[n] + data$HY[x]
				season_data$RC[n] <- season_data$RC[n] + data$HR[x]
				season_data$Corners[n] <- season_data$Corners[n] + data$HC[x]
				season_data$Fouls[n] <- season_data$Fouls[n] + data$HC[x]
				season_data$Shots[n] <- season_data$Shots[n] + data$HS[x]
				season_data$SonT[n] <- season_data$SonT[n] + data$HST[x]
				if(identical(data$FTR[x], "H")){
					season_data$Wins[n] <- season_data$Wins[n] + 1
				} else if(identical(data$FTR[x], "D")){
					season_data$Draws[n] <- season_data$Draws[n] + 1
				} else {
					season_data$Losses[n] <- season_data$Losses[n] + 1
				}
			} else if(identical(data$AwayTeam[x], season_data$Team[n])){
				season_data$Goals[n] <- season_data$Goals[n] + data$FTAG[x]
				season_data$Conceeded[n] <- season_data$Conceeded[n] + data$FTHG[x]
				season_data$YC[n] <- season_data$YC[n] + data$AY[x]
				season_data$RC[n] <- season_data$RC[n] + data$AR[x]
				season_data$Corners[n] <- season_data$Corners[n] + data$AC[x]
				season_data$Fouls[n] <- season_data$Fouls[n] + data$AC[x]
				season_data$Shots[n] <- season_data$Shots[n] + data$AS[x]
				season_data$SonT[n] <- season_data$SonT[n] + data$AST[x]
				if(identical(data$FTR[x], "A")){
					season_data$Wins[n] <- season_data$Wins[n] + 1
				} else if(identical(data$FTR[x], "D")){
					season_data$Draws[n] <- season_data$Draws[n] + 1
				} else {
					season_data$Losses[n] <- season_data$Losses[n] + 1
				}
			} else {
			}
		}
	}
	season_data$Points <- (season_data$Wins * 3) + season_data$Draws
	season_data$Games <- season_data$Wins + season_data$Draws + season_data$Losses
	season_data$Win.Rate <- (season_data$Wins / (season_data$Draws + season_data$Losses + season_data$Wins)) * 100
	season_data$GD <- season_data$Goals - season_data$Conceeded
	season_data$Shot.Accuracy <- (season_data$SonT / season_data$Shots) * 100
	season_data$Pos <- order(order(-season_data$Points, -season_data$GD, -season_data$Goals))

	season_data <- season_data[order(season_data$Pos),]

	season_path <-  "/Users/christian/Kaggle/Sports Analytics/Football/Prem Season Table Data"
	filename <- paste(season_path, "/", years[i], years[i-1],".csv", sep = "")
	write.csv(season_data, filename)
}

print('Completed Season Data')

for(i in 22:2){
	game_name <- paste(years[i], years[i-1], ".csv", sep = "")
	data <- read.csv(paste(path, "Champ Game Data/", game_name, sep = ""), stringsAsFactors = FALSE)
	Team <- unique(data$HomeTeam)
	season_data <- data.frame(Team)
	season_data$Team <- as.character(season_data$Team)
	season_data$Pos <- 0
	season_data$Points <- 0
	season_data$Games <- 0
	season_data$Wins <- 0
	season_data$Draws <- 0
	season_data$Losses <- 0
	season_data$Win.Rate <- 0
	season_data$YC <- 0
	season_data$RC <- 0
	season_data$Fouls <- 0
	season_data$Corners <- 0
	season_data$Goals <- 0
	season_data$Conceeded <- 0
	season_data$GD <- 0
	season_data$Shots <- 0
	season_data$SonT <- 0
	season_data$Shot.Accuracy <- 0
	for(n in 1:nrow(season_data)){
		for(x in nrow(data):1){
			if(identical(data$HomeTeam[x], season_data$Team[n])){
				season_data$Goals[n] <- season_data$Goals[n] + data$FTHG[x]
				season_data$Conceeded[n] <- season_data$Conceeded[n] + data$FTAG[x]
				season_data$YC[n] <- season_data$YC[n] + data$HY[x]
				season_data$RC[n] <- season_data$RC[n] + data$HR[x]
				season_data$Corners[n] <- season_data$Corners[n] + data$HC[x]
				season_data$Fouls[n] <- season_data$Fouls[n] + data$HC[x]
				season_data$Shots[n] <- season_data$Shots[n] + data$HS[x]
				season_data$SonT[n] <- season_data$SonT[n] + data$HST[x]
				if(identical(data$FTR[x], "H")){
					season_data$Wins[n] <- season_data$Wins[n] + 1
				} else if(identical(data$FTR[x], "D")){
					season_data$Draws[n] <- season_data$Draws[n] + 1
				} else {
					season_data$Losses[n] <- season_data$Losses[n] + 1
				}
			} else if(identical(data$AwayTeam[x], season_data$Team[n])){
				season_data$Goals[n] <- season_data$Goals[n] + data$FTAG[x]
				season_data$Conceeded[n] <- season_data$Conceeded[n] + data$FTHG[x]
				season_data$YC[n] <- season_data$YC[n] + data$AY[x]
				season_data$RC[n] <- season_data$RC[n] + data$AR[x]
				season_data$Corners[n] <- season_data$Corners[n] + data$AC[x]
				season_data$Fouls[n] <- season_data$Fouls[n] + data$AC[x]
				season_data$Shots[n] <- season_data$Shots[n] + data$AS[x]
				season_data$SonT[n] <- season_data$SonT[n] + data$AST[x]
				if(identical(data$FTR[x], "A")){
					season_data$Wins[n] <- season_data$Wins[n] + 1
				} else if(identical(data$FTR[x], "D")){
					season_data$Draws[n] <- season_data$Draws[n] + 1
				} else {
					season_data$Losses[n] <- season_data$Losses[n] + 1
				}
			} else {
			}
		}
	}
	season_data$Points <- (season_data$Wins * 3) + season_data$Draws
	season_data$Games <- season_data$Wins + season_data$Draws + season_data$Losses
	season_data$Win.Rate <- (season_data$Wins / (season_data$Draws + season_data$Losses + season_data$Wins)) * 100
	season_data$GD <- season_data$Goals - season_data$Conceeded
	season_data$Shot.Accuracy <- (season_data$SonT / season_data$Shots) * 100
	season_data$Pos <- order(order(-season_data$Points, -season_data$GD, -season_data$Goals))

	season_data <- season_data[order(season_data$Pos),]

	season_path <-  "/Users/christian/Kaggle/Sports Analytics/Football/Champ Season Table Data"
	filename <- paste(season_path, "/", years[i], years[i-1],".csv", sep = "")
	write.csv(season_data, filename)
}

print('Added Championship Data')

for(i in 22:3){
	game_name <- paste(years[i-1], years[i-2], ".csv", sep = "")
	game_data <- read.csv(paste(path, "Prem Game Data/", game_name, sep = ""), stringsAsFactors = FALSE)

	season_name <- paste(years[i], years[i-1], ".csv", sep = "")
	season_data <- read.csv(paste(path, "Prem Season Table Data/", season_name, sep = ""), stringsAsFactors = FALSE)

	champ_name <- paste(years[i], years[i-1], ".csv", sep = "")
	champ_data <- read.csv(paste(path, "Champ Season Table Data/", champ_name, sep = ""), stringsAsFactors = FALSE)

	game_data$HomeTeam <- as.character(game_data$HomeTeam)
	game_data$AwayTeam <- as.character(game_data$AwayTeam)
	game_data$Home.New.Team <- 0
	game_data$Away.New.Team <- 0
	game_data$Home.Pos.last.season <- NA
	game_data$Away.Pos.last.season <- NA
	game_data$Home.Points.last.season <- NA
	game_data$Away.Points.last.season <- NA
	game_data$Home.Goals.last.season <- NA
	game_data$Away.Goals.last.season <- NA
	game_data$Home.Conceeded.last.season <- NA
	game_data$Away.Conceeded.last.season <- NA
	game_data$Home.Win.Rate.last.season <- NA
	game_data$Away.Win.Rate.last.season <- NA
	game_data$Home.YC.last.season <- NA
	game_data$Away.YC.last.season <- NA
	game_data$Home.RC.last.season <- NA
	game_data$Away.RC.last.season <- NA
	game_data$Home.Fouls.last.season <- NA
	game_data$Away.Fouls.last.season <- NA
	game_data$Home.Corners.last.season <- NA
	game_data$Away.Corners.last.season <- NA
	game_data$Home.Shots.last.season <- NA
	game_data$Away.Shots.last.season <- NA
	game_data$Home.Shots.on.Target.last.season <- NA
	game_data$Away.Shots.on.Target.last.season <- NA
	game_data$Home.Shot.Accuracy.last.season <- NA
	game_data$Away.Shot.Accuracy.last.season <- NA
	game_data$Home.Running.Total.Points <- 0
	game_data$Away.Running.Total.Points <- 0
	game_data$Home.Running.Total.Goals <- 0
	game_data$Away.Running.Total.Goals <- 0
	game_data$Home.Running.Total.Conceeded <- 0
	game_data$Away.Running.Total.Conceeded <- 0
	game_data$Home.Running.Total.Wins <- 0
	game_data$Away.Running.Total.Wins <- 0
	game_data$Home.Running.Total.Draws <- 0
	game_data$Away.Running.Total.Draws <- 0
	game_data$Home.Running.Total.Losses <- 0
	game_data$Away.Running.Total.Losses <- 0
	game_data$Home.Running.Total.YC <- 0
	game_data$Away.Running.Total.YC <- 0
	game_data$Home.Running.Total.RC <- 0
	game_data$Away.Running.Total.RC <- 0
	game_data$Home.Running.Total.Fouls <- 0
	game_data$Away.Running.Total.Fouls <- 0
	game_data$Home.Running.Total.Corners <- 0
	game_data$Away.Running.Total.Corners <- 0
	game_data$Home.Running.Total.Shots <- 0
	game_data$Away.Running.Total.Shots <- 0
	game_data$Home.Running.Total.Shots.on.Target <- 0
	game_data$Away.Running.Total.Shots.on.Target <- 0
	for(j in nrow(game_data):1){
		for(n in nrow(season_data):1){
			if(identical(game_data$HomeTeam[j], season_data$Team[n])){
				game_data$Home.Pos.last.season[j] <- season_data$Pos[n]
				game_data$Home.Points.last.season[j] <- season_data$Points[n]
				game_data$Home.Goals.last.season[j] <- season_data$Goals[n]
				game_data$Home.Conceeded.last.season[j] <- season_data$Conceeded[n]
				game_data$Home.Win.Rate.last.season[j] <- season_data$Win.Rate[n]
				game_data$Home.YC.last.season[j] <- season_data$YC[n]
				game_data$Home.RC.last.season[j] <- season_data$RC[n]
				game_data$Home.Fouls.last.season[j] <- season_data$Fouls[n]
				game_data$Home.Corners.last.season[j] <- season_data$Corners[n]
				game_data$Home.Shots.last.season[j] <- season_data$Shots[n]
				game_data$Home.Shots.on.Target.last.season[j] <- season_data$SonT[n]
				game_data$Home.Shot.Accuracy.last.season[j] <- season_data$Shot.Accuracy[n]
			} else {
			}
			if(identical(game_data$AwayTeam[j], season_data$Team[n])){
				game_data$Away.Pos.last.season[j] <- season_data$Pos[n]
				game_data$Away.Points.last.season[j] <- season_data$Points[n]
				game_data$Away.Goals.last.season[j] <- season_data$Goals[n]
				game_data$Away.Conceeded.last.season[j] <- season_data$Conceeded[n]
				game_data$Away.Win.Rate.last.season[j] <- season_data$Win.Rate[n]
				game_data$Away.YC.last.season[j] <- season_data$YC[n]
				game_data$Away.RC.last.season[j] <- season_data$RC[n]
				game_data$Away.Fouls.last.season[j] <- season_data$Fouls[n]
				game_data$Away.Corners.last.season[j] <- season_data$Corners[n]
				game_data$Away.Shots.last.season[j] <- season_data$Shots[n]
				game_data$Away.Shots.on.Target.last.season[j] <- season_data$SonT[n]
				game_data$Away.Shot.Accuracy.last.season[j] <- season_data$Shot.Accuracy[n]
			} else {
			}
		}
		for(n in 1:nrow(champ_data)){
			if(identical(champ_data$Team[n], game_data$HomeTeam[j])){
				game_data$Home.New.Team[j] <- 1
				game_data$Home.Pos.last.season[j] <- champ_data$Pos[n]
				game_data$Home.Points.last.season[j] <- champ_data$Points[n]
				game_data$Home.Goals.last.season[j] <- champ_data$Goals[n]
				game_data$Home.Conceeded.last.season[j] <- champ_data$Conceeded[n]
				game_data$Home.Win.Rate.last.season[j] <- champ_data$Win.Rate[n]
				game_data$Home.YC.last.season[j] <- champ_data$YC[n]
				game_data$Home.RC.last.season[j] <- champ_data$RC[n]
				game_data$Home.Fouls.last.season[j] <- champ_data$Fouls[n]
				game_data$Home.Corners.last.season[j] <- champ_data$Corners[n]
				game_data$Home.Shots.last.season[j] <- champ_data$Shots[n]
				game_data$Home.Shots.on.Target.last.season[j] <- champ_data$SonT[n]
				game_data$Home.Shot.Accuracy.last.season[j] <- champ_data$Shot.Accuracy[n]
			} else {
			}
			if(identical(champ_data$Team[n], game_data$AwayTeam[j])){
				game_data$Away.New.Team[j] <- 1
				game_data$Away.Pos.last.season[j] <- champ_data$Pos[n]
				game_data$Away.Points.last.season[j] <- champ_data$Points[n]
				game_data$Away.Goals.last.season[j] <- champ_data$Goals[n]
				game_data$Away.Conceeded.last.season[j] <- champ_data$Conceeded[n]
				game_data$Away.Win.Rate.last.season[j] <- champ_data$Win.Rate[n]
				game_data$Away.YC.last.season[j] <- champ_data$YC[n]
				game_data$Away.RC.last.season[j] <- champ_data$RC[n]
				game_data$Away.Fouls.last.season[j] <- champ_data$Fouls[n]
				game_data$Away.Corners.last.season[j] <- champ_data$Corners[n]
				game_data$Away.Shots.last.season[j] <- champ_data$Shots[n]
				game_data$Away.Shots.on.Target.last.season[j] <- champ_data$SonT[n]
				game_data$Away.Shot.Accuracy.last.season[j] <- champ_data$Shot.Accuracy[n]
			} else {
			}
		}
	}
	write.csv(game_data, paste(path, "Processed Game Data/", game_name, sep = ""))
}

print('Added Cummulative Data')

for(i in 22:3){
	current_data_name <- paste(years[i-1], years[i-2], ".csv", sep = "")
	current_data <- read.csv(paste(path, "Processed Game Data/", current_data_name, sep = ""), stringsAsFactors = FALSE)
	for(j in 2:nrow(current_data)){
		for(n in 1:(j-1)){
			if(identical(current_data$HomeTeam[j], current_data$HomeTeam[n])){
				if(identical(current_data$FTR[n], "H")){
					current_data$Home.Running.Total.Wins[j] <- current_data$Home.Running.Total.Wins[j] + 1
				} else if(identical(current_data$FTR[n], "D")){
					current_data$Home.Running.Total.Draws[j] <- current_data$Home.Running.Total.Draws[j] + 1
				} else if(identical(current_data$FTR[n], "A")){
					current_data$Home.Running.Total.Losses[j] <- current_data$Home.Running.Total.Losses[j] + 1
				}
				current_data$Home.Running.Total.Shots[j] <- current_data$Home.Running.Total.Shots[j] + current_data$HS[n]
				current_data$Home.Running.Total.Corners[j] <- current_data$Home.Running.Total.Corners[j] + current_data$HC[n]
				current_data$Home.Running.Total.Fouls[j] <- current_data$Home.Running.Total.Fouls[j] + current_data$HF[n]
				current_data$Home.Running.Total.YC[j] <- current_data$Home.Running.Total.YC[j] + current_data$HY[n]
				current_data$Home.Running.Total.RC[j] <- current_data$Home.Running.Total.RC[j] + current_data$HR[n]
				current_data$Home.Running.Total.Conceeded[j] <- current_data$Home.Running.Total.Conceeded[j] + current_data$FTAG[n]
				current_data$Home.Running.Total.Shots.on.Target[j] <- current_data$Home.Running.Total.Shots.on.Target[j] + current_data$HST[n]
			} else if(identical(current_data$HomeTeam[j], current_data$AwayTeam[n])){
				if(identical(current_data$FTR[n], "A")){
					current_data$Home.Running.Total.Wins[j] <- current_data$Home.Running.Total.Wins[j] + 1
				} else if(identical(current_data$FTR[n], "D")){
					current_data$Home.Running.Total.Draws[j] <- current_data$Home.Running.Total.Draws[j] + 1
				} else if(identical(current_data$FTR[n], "H")){
					current_data$Home.Running.Total.Losses[j] <- current_data$Home.Running.Total.Losses[j]+ 1
				}
				current_data$Home.Running.Total.Shots[j] <- current_data$Home.Running.Total.Shots[j] + current_data$AS[n]
				current_data$Home.Running.Total.Corners[j] <- current_data$Home.Running.Total.Corners[j] + current_data$AC[n]
				current_data$Home.Running.Total.Fouls[j] <- current_data$Home.Running.Total.Fouls[j] + current_data$AF[n]
				current_data$Home.Running.Total.YC[j] <- current_data$Home.Running.Total.YC[j] + current_data$AY[n]
				current_data$Home.Running.Total.RC[j] <- current_data$Home.Running.Total.RC[j] + current_data$AR[n]
				current_data$Home.Running.Total.Conceeded[j] <- current_data$Home.Running.Total.Conceeded[j] + current_data$FTHG[n]
				current_data$Home.Running.Total.Shots.on.Target[j] <- current_data$Home.Running.Total.Shots.on.Target[j] + current_data$AST[n]
			}
			if(identical(current_data$AwayTeam[j], current_data$HomeTeam[n])){
				if(identical(current_data$FTR[n], "H")){
					current_data$Away.Running.Total.Wins[j] <- current_data$Away.Running.Total.Wins[j] + 1
				} else if(identical(current_data$FTR[n], "D")){
					current_data$Away.Running.Total.Draws[j] <- current_data$Away.Running.Total.Draws[j] + 1
				} else if(identical(current_data$FTR[n], "A")){
					current_data$Away.Running.Total.Losses[j] <- current_data$Away.Running.Total.Losses[j] + 1
				}
				current_data$Away.Running.Total.Shots[j] <- current_data$Away.Running.Total.Shots[j] + current_data$HS[n]
				current_data$Away.Running.Total.Corners[j] <- current_data$Away.Running.Total.Corners[j] + current_data$HC[n]
				current_data$Away.Running.Total.Fouls[j] <- current_data$Away.Running.Total.Fouls[j] + current_data$HF[n]
				current_data$Away.Running.Total.YC[j] <- current_data$Away.Running.Total.YC[j] + current_data$HY[n]
				current_data$Away.Running.Total.RC[j] <- current_data$Away.Running.Total.RC[j] + current_data$HR[n]
				current_data$Away.Running.Total.Conceeded[j] <- current_data$Away.Running.Total.Conceeded[j] + current_data$FTAG[n]
				current_data$Away.Running.Total.Shots.on.Target[j] <- current_data$Away.Running.Total.Shots.on.Target[j] + current_data$HST[n]
			} else if(identical(current_data$AwayTeam[j], current_data$AwayTeam[n])){
				if(identical(current_data$FTR[n], "A")){
					current_data$Away.Running.Total.Wins[j] <- current_data$Away.Running.Total.Wins[j] + 1
				} else if(identical(current_data$FTR[n], "D")){
					current_data$Away.Running.Total.Draws[j] <- current_data$Away.Running.Total.Draws[j] + 1
				} else if(identical(current_data$FTR[n], "H")){
					current_data$Away.Running.Total.Losses[j] <- current_data$Away.Running.Total.Losses[j] + 1
				}
				current_data$Away.Running.Total.Shots[j] <- current_data$Away.Running.Total.Shots[j] + current_data$AS[n]
				current_data$Away.Running.Total.Corners[j] <- current_data$Away.Running.Total.Corners[j] + current_data$AC[n]
				current_data$Away.Running.Total.Fouls[j] <- current_data$Away.Running.Total.Fouls[j] + current_data$AF[n]
				current_data$Away.Running.Total.YC[j] <- current_data$Away.Running.Total.YC[j] + current_data$AY[n]
				current_data$Away.Running.Total.RC[j] <- current_data$Away.Running.Total.RC[j] + current_data$AR[n]
				current_data$Away.Running.Total.Conceeded[j] <- current_data$Away.Running.Total.Conceeded[j] + current_data$FTHG[n]
				current_data$Away.Running.Total.Shots.on.Target[j] <- current_data$Away.Running.Total.Shots.on.Target[j] + current_data$AST[n]
			} else {
			}
		}
	}
	current_data$Home.Running.Total.Points <- (current_data$Home.Running.Total.Wins * 3) + current_data$Home.Running.Total.Draws
	current_data$Away.Running.Total.Points <- (current_data$Away.Running.Total.Wins * 3) + current_data$Away.Running.Total.Draws

	write.csv(current_data, paste(path, "Processed Game Data/", current_data_name, sep = ""))
}

print('Completed Processed Data')

Teams <- fromJSON(file = "teams.json")
winter_transfer_date <- "31/01/"

for(i in 22:2){
	transfer_name <- paste(years[i], years[i-1], ".csv", sep = "")
	transfer_data <- read.csv(paste(path, "Prem Transfer Data/", transfer_name, sep = ""), stringsAsFactors = FALSE)
	for(j in 1:length(Teams)){
		for(n in 1:nrow(transfer_data)){
			if(str_detect(Teams[j], transfer_data$club_name[n], negate = FALSE) || str_detect(transfer_data$club_name[n], Teams[j], negate = FALSE)){
				transfer_data$club_name[n] <- Teams[j]
			} else {
			}
		}
	}
	Team <- as.array(unique(transfer_data$club_name))
	new <- data.frame(Team)

	new$Total.Spent.Summer <- 0
	new$Total.Spent.Winter <- 0
	new$Total.Sold.Summer <- 0
	new$Total.Sold.Winter <- 0
	new$Net.Spend.Summer <- 0
	new$Net.Spend.Winter <- 0
	new$Avg.Spend.Summer <-  0
	new$Avg.Sold.Summer <- 0
	new$Avg.Spend.Winter <- 0
	new$Avg.Sold.Winter <- 0

	new$Defensive.Spend.Summer <- 0
	new$Defensive.Sold.Summer <- 0
	new$Defensive.Spend.Winter <- 0
	new$Defensive.Sold.Winter <- 0
	new$Midfield.Spend.Summer <- 0
	new$Midfield.Sold.Summer <- 0
	new$Midfield.Spend.Winter <- 0
	new$Midfield.Sold.Winter <- 0
	new$Forward.Spend.Summer <- 0
	new$Forward.Sold.Summer <- 0
	new$Forward.Spend.Winter <- 0
	new$Forward.Sold.Winter <- 0

	new$Total.Players.In.Summer <- 0
	new$Total.Players.Out.Summer <- 0
	new$Total.Players.In.Winter <- 0
	new$Total.Players.Out.Winter <- 0
	new$Defensive.Players.In.Summer <- 0
	new$Midfield.Players.In.Summer <- 0
	new$Forward.Players.In.Summer <- 0
	new$Defensive.Players.Out.Summer <- 0
	new$Midfield.Players.Out.Summer <- 0
	new$Forward.Players.Out.Summer <- 0
	new$Defensive.Players.In.Winter <- 0
	new$Midfield.Players.In.Winter <- 0
	new$Forward.Players.In.Winter <- 0
	new$Defensive.Players.Out.Winter <- 0
	new$Midfield.Players.Out.Winter <- 0
	new$Forward.Players.Out.Winter <- 0

	new$Total.Age.Players.In.Summer <- 0
	new$Total.Age.Players.Out.Summer <- 0
	new$Total.Age.Players.In.Winter <- 0
	new$Total.Age.Players.Out.Winter <- 0
	new$Avg.Age.Players.In.Summer <- 0
	new$Avg.Age.Players.Out.Summer <- 0
	new$Avg.Age.Players.In.Winter <-  0
	new$Avg.Age.Players.Out.Winter <- 0
	for(j in 1:nrow(new)){
		for(n in 1:nrow(transfer_data)){
			if(is.na(transfer_data$fee_cleaned[n])){
				transfer_data$fee_cleaned[n] <- 0
			} else {
			}
			if(identical(new$Team[j], transfer_data$club_name[n])){
				if(identical(transfer_data$transfer_period[n], "Summer")){
					if(identical(transfer_data$transfer_movement[n], "in")){
						new$Total.Players.In.Summer[j] <- new$Total.Players.In.Summer[j] + 1
						new$Total.Spent.Summer[j] <- new$Total.Spent.Summer[j] + transfer_data$fee_cleaned[n]
						new$Total.Age.Players.In.Summer[j] <- new$Total.Age.Players.In.Summer[j] + transfer_data$age[n]
						if(str_detect(transfer_data$position[n], "Back") || str_detect(transfer_data$position[n], "Goalkeeper") || str_detect(transfer_data$position[n], "Defender") || str_detect(transfer_data$position[n], "Sweeper")){
							new$Defensive.Players.In.Summer[j] <- new$Defensive.Players.In.Summer[j] + 1
							new$Defensive.Spend.Summer[j] <- new$Defensive.Spend.Summer[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Midfield")){
							new$Midfield.Players.In.Summer[j] <- new$Midfield.Players.In.Summer[j] + 1
							new$Midfield.Spend.Summer[j] <- new$Midfield.Spend.Summer[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Winger") || str_detect(transfer_data$position[n], "Forward") || str_detect(transfer_data$position[n], "Striker")){
							new$Forward.Players.In.Summer[j] <- new$Forward.Players.In.Summer[j] + 1
							new$Forward.Spend.Summer[j] <- new$Forward.Spend.Summer[j] + transfer_data$fee_cleaned[n]
						} else {
							print(sprintf("I don't know what a %s is", transfer_data$position[n]))
						} 
					} else if(identical(transfer_data$transfer_movement[n], "out")){
						new$Total.Players.Out.Summer[j] <- new$Total.Players.Out.Summer[j] + 1
						new$Total.Sold.Summer[j] <- new$Total.Sold.Summer[j] + transfer_data$fee_cleaned[n]
						new$Total.Age.Players.Out.Summer[j] <- new$Total.Age.Players.Out.Summer[j] + transfer_data$age[n]
						if(str_detect(transfer_data$position[n], "Back") || str_detect(transfer_data$position[n], "Goalkeeper") || str_detect(transfer_data$position[n], "Defender") || str_detect(transfer_data$position[n], "Sweeper")){
							new$Defensive.Players.Out.Summer[j] <- new$Defensive.Players.In.Summer[j] + 1
							new$Defensive.Sold.Summer[j] <- new$Defensive.Sold.Summer[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Midfield")){
							new$Midfield.Players.Out.Summer[j] <- new$Midfield.Players.Out.Summer[j] + 1
							new$Midfield.Sold.Summer[j] <- new$Midfield.Sold.Summer[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Winger") || str_detect(transfer_data$position[n], "Forward") || str_detect(transfer_data$position[n], "Striker")){
							new$Forward.Players.Out.Summer[j] <- new$Forward.Players.Out.Summer[j] + 1
							new$Forward.Sold.Summer[j] <- new$Forward.Sold.Summer[j] + transfer_data$fee_cleaned[n]
						} else {
							print(sprintf("I don't know what a %s is", transfer_data$position[n]))
						}
					}
				} else if(identical(transfer_data$transfer_period[n], "Winter")){
					if(identical(transfer_data$transfer_movement[n], "in")){
						new$Total.Players.In.Winter[j] <- new$Total.Players.In.Winter[j] + 1
						new$Total.Spent.Winter[j] <- new$Total.Spent.Winter[j] + transfer_data$fee_cleaned[n]
						new$Total.Age.Players.In.Winter[j] <- new$Total.Age.Players.In.Winter[j] + transfer_data$age[n]
						if(str_detect(transfer_data$position[n], "Back") || str_detect(transfer_data$position[n], "Goalkeeper") || str_detect(transfer_data$position[n], "Defender") || str_detect(transfer_data$position[n], "Sweeper")){
							new$Defensive.Players.In.Winter[j] <- new$Defensive.Players.In.Winter[j] + 1
							new$Defensive.Spend.Winter[j] <- new$Defensive.Spend.Winter[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Midfield")){
							new$Midfield.Players.In.Winter[j] <- new$Midfield.Players.In.Winter[j] + 1
							new$Midfield.Spend.Winter[j] <- new$Midfield.Spend.Winter[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Winger") || str_detect(transfer_data$position[n], "Forward") || str_detect(transfer_data$position[n], "Striker")){
							new$Forward.Players.In.Winter[j] <- new$Forward.Players.In.Winter[j] + 1
							new$Forward.Spend.Winter[j] <- new$Forward.Spend.Winter[j] + transfer_data$fee_cleaned[n]
						} else {
							print(sprintf("I don't know what a %s is", transfer_data$position[n]))
						}
					} else if(identical(transfer_data$transfer_movement[n], "out")){
						new$Total.Players.Out.Winter[j] <- new$Total.Players.Out.Winter[j] + 1
						new$Total.Sold.Winter[j] <- new$Total.Sold.Winter[j] + transfer_data$fee_cleaned[n]
						new$Total.Age.Players.Out.Winter[j] <- new$Total.Age.Players.Out.Winter[j] + transfer_data$age[n]
						if(str_detect(transfer_data$position[n], "Back") || str_detect(transfer_data$position[n], "Goalkeeper") || str_detect(transfer_data$position[n], "Defender") || str_detect(transfer_data$position[n], "Sweeper")){
							new$Defensive.Players.Out.Winter[j] <- new$Defensive.Players.Out.Winter[j] + 1
							new$Defensive.Sold.Winter[j] <- new$Defensive.Sold.Winter[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Midfield")){
							new$Midfield.Players.Out.Winter[j] <- new$Midfield.Players.Out.Winter[j] + 1
							new$Midfield.Sold.Winter[j] <- new$Midfield.Sold.Winter[j] + transfer_data$fee_cleaned[n]
						} else if(str_detect(transfer_data$position[n], "Winger") || str_detect(transfer_data$position[n], "Forward") || str_detect(transfer_data$position[n], "Striker")){
							new$Forward.Players.Out.Winter[j] <- new$Forward.Players.Out.Winter[j] + 1
							new$Forward.Sold.Winter[j] <- new$Forward.Sold.Winter[j] + transfer_data$fee_cleaned[n]
						} else {
							print(sprintf("I don't know what a %s is", transfer_data$position[n]))
						}
					} else {
					}
				} else {
				}
			} else {
			}
		}
	}
	new$Net.Spend.Summer <- new$Total.Spent.Summer - new$Total.Sold.Summer
	new$Net.Spend.Winter <- new$Total.Spent.Winter - new$Total.Sold.Winter
	new$Avg.Spend.Summer <- new$Total.Spent.Summer / new$Total.Players.In.Summer
	new$Avg.Spend.Winter <- new$Total.Spent.Winter / new$Total.Players.In.Winter
	new$Avg.Sold.Summer <- new$Total.Sold.Summer / new$Total.Players.Out.Summer
	new$Avg.Sold.Winter <- new$Total.Sold.Winter / new$Total.Players.Out.Winter
	new$Avg.Age.Players.In.Summer <- new$Avg.Age.Players.In.Summer / new$Total.Players.In.Summer
	new$Avg.Age.Players.Out.Summer <- new$Avg.Age.Players.Out.Summer / new$Total.Players.Out.Summer
	new$Avg.Age.Players.In.Winter <- new$Avg.Age.Players.In.Winter / new$Total.Players.In.Winter
	new$Avg.Age.Players.Out.Winter <- new$Avg.Age.Players.Out.Winter / new$Total.Players.Out.Winter
	new$Prop.Defensive.Players.Bought.Summer <- (new$Defensive.Players.In.Summer / new$Total.Players.In.Summer) * 100
	new$Prop.Defensive.Players.Sold.Summer <- (new$Defensive.Players.Out.Summer / new$Total.Players.Out.Summer) * 100
	new$Prop.Midfield.Players.Bought.Summer <- (new$Midfield.Players.In.Summer / new$Total.Players.In.Summer) * 100
	new$Prop.Midfield.Players.Sold.Summer <- (new$Midfield.Players.Out.Summer / new$Total.Players.Out.Summer) * 100
	new$Prop.Forward.Players.Bought.Summer <- (new$Forward.Players.In.Summer / new$Total.Players.In.Summer) * 100
	new$Prop.Forward.Players.Sold.Summer <- (new$Forward.Players.Out.Summer / new$Total.Players.Out.Summer) * 100
	new$Prop.Defensive.Players.Bought.Winter <- (new$Defensive.Players.In.Winter / new$Total.Players.In.Winter) * 100
	new$Prop.Defensive.Players.Sold.Winter <- (new$Defensive.Players.Out.Winter / new$Total.Players.Out.Winter) * 100
	new$Prop.Midfield.Players.Bought.Winter <-(new$Midfield.Players.In.Winter / new$Total.Players.In.Winter) * 100
	new$Prop.Midfield.Players.Sold.Winter <- (new$Midfield.Players.Out.Winter / new$Total.Players.Out.Winter) * 100
	new$Prop.Forward.Players.Bought.Winter <- (new$Forward.Players.In.Winter / new$Total.Players.In.Winter) * 100
	new$Prop.Forward.Players.Sold.Winter <- (new$Forward.Players.Out.Winter / new$Total.Players.Out.Winter) * 100
	new$Avg.Age.Players.In.Summer <- new$Total.Age.Players.In.Summer / new$Total.Players.In.Summer
	new$Avg.Age.Players.Out.Summer <- new$Total.Age.Players.Out.Summer / new$Total.Players.Out.Summer
	new$Avg.Age.Players.In.Summer <- new$Total.Age.Players.In.Winter / new$Total.Players.In.Winter
	new$Avg.Age.Players.Out.Summer <- new$Total.Age.Players.Out.Winter / new$Total.Players.Out.Winter
	new$Net.Spend.Total <- (new$Total.Spent.Summer + new$Total.Spent.Winter) - (new$Total.Sold.Summer + new$Total.Sold.Winter)
	new$Avg.Spend.Total <- (new$Total.Spent.Summer + new$Total.Spent.Winter) / (new$Total.Players.Out.Summer + new$Total.Players.Out.Winter)
	new$Avg.Sold.Total <- (new$Total.Sold.Summer + new$Total.Sold.Winter) / (new$Total.Players.Out.Summer + new$Total.Players.Out.Winter)
	new$Avg.Age.Players.In.Total <- (new$Total.Age.Players.In.Summer + new$Total.Age.Players.In.Winter) / (new$Total.Players.In.Summer + new$Total.Players.In.Winter)
	new$Avg.Age.Players.Out.Total <- (new$Total.Age.Players.Out.Summer + new$Total.Age.Players.Out.Winter) / (new$Total.Players.Out.Summer + new$Total.Players.Out.Winter)
	
	new[is.na(new)] <- 0
	write.csv(new, paste(path, "/Prem Transfer Data/", transfer_name, sep = ""))
}

print('Completed Transfer Data')

for(i in 22:3){
	game_name <- paste(years[i-1], years[i-2], ".csv", sep = "")
	game_data <- read.csv(paste(path, "Processed Game Data/", game_name, sep = ""), stringsAsFactors = FALSE)

	transfer_name <- paste(years[i-1], years[i-2], ".csv", sep = "")
	transfer_data <- read.csv(paste(path, "Prem Transfer Data/", game_name, sep = ""), stringsAsFactors = FALSE)

	game_data$Home.Total.Players.In.So.Far <- 0
	game_data$Away.Total.Players.In.So.Far <- 0
	game_data$Home.Total.Players.Out.So.Far <- 0
	game_data$Away.Total.Players.Out.So.Far <- 0
	game_data$Home.Net.Player.Movement.So.Far <- 0
	game_data$Home.Total.Spend.So.Far <- 0
	game_data$Away.Total.Spend.So.Far <- 0
	game_data$Home.Total.Sold.So.Far <- 0
	game_data$Away.Total.Sold.So.Far <- 0
	game_data$Home.Net.Spend.So.Far <- 0
	game_data$Away.Net.Spend.So.Far <- 0
	game_data$Home.Avg.Spent.Per.Player.So.Far <- 0
	game_data$Away.Avg.Spent.Per.Player.So.Far <- 0
	game_data$Home.Avg.Sold.Per.Player.So.Far <- 0
	game_data$Away.Avg.Sold.Per.Player.So.Far <- 0
	game_data$Home.Avg.Age.Players.In.So.Far <- 0
	game_data$Away.Avg.Age.Players.In.So.Far <- 0
	game_data$Home.Avg.Age.Players.Out.So.Far <- 0
	game_data$Away.Avg.Age.Players.Out.So.Far <- 0
	game_data$Home.Defensive.Players.In.So.Far <- 0
	game_data$Away.Defensive.Players.Out.So.Far <- 0
	game_data$Home.Midfield.Players.In.So.Far <- 0
	game_data$Away.Midfield.Players.Out.So.Far <- 0
	game_data$Home.Forward.Players.In.So.Far <- 0
	game_data$Away.Forward.Players.Out.So.Far <- 0
	game_data$Home.Prop.Defensive.Players.In.So.Far <- 0
	game_data$Away.Prop.Defensive.Players.Out.So.Far <- 0
	game_data$Home.Prop.Midfield.Players.In.So.Far <- 0
	game_data$Away.Prop.Midfield.Players.Out.So.Far <- 0
	game_data$Home.Prop.Forward.Players.In.So.Far <- 0
	game_data$Away.Prop.Forward.Players.Out.So.Far <- 0

	for(j in 1:nrow(game_data)){
		for(n in 1:nrow(transfer_data)){
			if(nchar(game_data$Date[j]) == 8){
				date <- as.Date(game_data$Date[j], format = "%d/%m/%y")
			} else {
				date <- as.Date(game_data$Date[j], format = "%d/%m/%Y")
			}
			deadline_date <- as.Date(paste("31/01/20", years[i-1], sep = ""), "%d/%m/%Y")
			if(identical(game_data$HomeTeam[j], transfer_data$Team[n])){
				if(date > deadline_date){
					game_data$Home.Total.Players.In.So.Far[j] <- transfer_data$Total.Players.In.Winter[n] + transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Total.Players.Out.So.Far[j] <- transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n]
					game_data$Home.Net.Player.Movement.So.Far[j] <- (transfer_data$Total.Players.In.Summer[n] + transfer_data$Total.Players.In.Winter[n]) - (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Home.Total.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Players.In.Winter[n]
					game_data$Home.Total.Sold.So.Far[j] <- transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n]
					game_data$Home.Net.Spend.So.Far[j] <- (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n]) - (transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n])
					game_data$Home.Avg.Spent.Per.Player.So.Far[j] <- (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n]) / (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n])
					game_data$Home.Avg.Sold.Per.Player.So.Far[j] <- (transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n]) / (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Home.Avg.Age.Players.In.So.Far[j] <- transfer_data$Total.Age.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Avg.Age.Players.Out.So.Far[j] <- transfer_data$Total.Age.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n]
					game_data$Home.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n]
					game_data$Home.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n]
					game_data$Home.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.Out.Summer[n]
					game_data$Home.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n]
					game_data$Home.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n]
					game_data$Home.Prop.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Prop.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Prop.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
				} else {
					game_data$Home.Total.Players.In.So.Far[j] <- transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Total.Players.Out.So.Far[j] <- transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Net.Player.Movement.So.Far[j] <- transfer_data$Total.Players.In.Summer[n] - transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Total.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n]
					game_data$Home.Total.Sold.So.Far[j] <- transfer_data$Total.Sold.Summer[n]
					game_data$Home.Net.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n] - transfer_data$Total.Sold.Summer[n]
					game_data$Home.Avg.Spent.Per.Player.So.Far[j] <- transfer_data$Total.Spent.Summer[n] / transfer_data$Total.Spent.Summer[n]
					game_data$Home.Avg.Sold.Per.Player.So.Far[j] <- transfer_data$Total.Sold.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Avg.Age.Players.In.So.Far[j] <- (transfer_data$Total.Age.Players.In.Summer[n] + transfer_data$Total.Age.Players.In.Winter[n]) / (transfer_data$Total.Players.In.Summer[n] + transfer_data$Total.Players.In.Winter[n])
					game_data$Home.Avg.Age.Players.Out.So.Far[j] <- (transfer_data$Total.Age.Players.Out.Summer[n] + transfer_data$Total.Age.Players.Out.Winter[n]) / (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Home.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n]
					game_data$Home.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n]
					game_data$Home.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n]
					game_data$Home.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.Out.Summer[n]
					game_data$Home.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n]
					game_data$Home.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n]
					game_data$Home.Prop.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Home.Prop.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Prop.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Home.Prop.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
				}
			} else if(identical(game_data$AwayTeam[j], transfer_data$Team[n])){
				if(game_data$Date[j] > date){
					game_data$Away.Total.Players.In.So.Far[j] <- transfer_data$Total.Players.In.Winter[n] + transfer_data$Total.Players.In.Summer[n]
					game_data$Away.Total.Players.Out.So.Far[j] <- transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n]
					game_data$Away.Net.Player.Movement.So.Far[j] <- (transfer_data$Total.Players.In.Summer[n] + transfer_data$Total.Players.In.Winter[n]) - (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Away.Total.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Players.In.Winter[n]
					game_data$Away.Total.Sold.So.Far[j] <- transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n]
					game_data$Away.Net.Spend.So.Far[j] <- (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n]) - (transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n])
					game_data$Away.Avg.Spent.Per.Player.So.Far[j] <- (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n]) / (transfer_data$Total.Spent.Summer[n] + transfer_data$Total.Spent.Winter[n])
					game_data$Away.Avg.Sold.Per.Player.So.Far[j] <- (transfer_data$Total.Sold.Summer[n] + transfer_data$Total.Sold.Winter[n]) / (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Away.Avg.Age.Players.In.So.Far[j] <- transfer_data$Total.Age.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]
					game_data$Away.Avg.Age.Players.Out.So.Far[j] <- transfer_data$Total.Age.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Away.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n]
					game_data$Away.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n]
					game_data$Away.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n]
					game_data$Away.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.Out.Summer[n]
					game_data$Away.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n]
					game_data$Away.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n]
					game_data$Away.Prop.Defensive.Players.In.So.Far[j] <- (transfer_data$Defensive.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Midfield.Players.In.So.Far[j] <- (transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Forward.Players.In.So.Far[j] <- (transfer_data$Forward.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Defensive.Players.Out.So.Far[j] <- (transfer_data$Defensive.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
					game_data$Away.Prop.Midfield.Players.Out.So.Far[j] <- (transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
					game_data$Away.Prop.Forward.Players.Out.So.Far[j] <- (transfer_data$Forward.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
				} else {
					game_data$Away.Total.Players.In.So.Far[j] <- transfer_data$Total.Players.In.Summer[n]
					game_data$Away.Total.Players.Out.So.Far[j] <- transfer_data$Total.Players.Out.Summer[n]
					game_data$Away.Net.Player.Movement.So.Far[j] <- transfer_data$Total.Players.In.Summer[n] - transfer_data$Total.Players.Out.Summer[n]
					game_data$Away.Total.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n]
					game_data$Away.Total.Sold.So.Far[j] <- transfer_data$Total.Sold.Summer[n]
					game_data$Away.Net.Spend.So.Far[j] <- transfer_data$Total.Spent.Summer[n] - transfer_data$Total.Sold.Summer[n]
					game_data$Away.Avg.Spent.Per.Player.So.Far[j] <- transfer_data$Total.Spent.Summer[n] / transfer_data$Total.Spent.Summer[n]
					game_data$Away.Avg.Sold.Per.Player.So.Far[j] <- transfer_data$Total.Sold.Summer[n] / transfer_data$Total.Players.Out.Summer[n]
					game_data$Away.Avg.Age.Players.In.So.Far[j] <- (transfer_data$Total.Age.Players.In.Summer[n] + transfer_data$Total.Age.Players.In.Winter[n]) / (transfer_data$Total.Players.In.Summer[n] + transfer_data$Total.Players.In.Winter[n])
					game_data$Away.Avg.Age.Players.Out.So.Far[j] <- (transfer_data$Total.Age.Players.Out.Summer[n] + transfer_data$Total.Age.Players.Out.Winter[n]) / (transfer_data$Total.Players.Out.Summer[n] + transfer_data$Total.Players.Out.Winter[n])
					game_data$Away.Defensive.Players.In.So.Far[j] <- transfer_data$Defensive.Players.In.Summer[n]
					game_data$Away.Defensive.Players.Out.So.Far[j] <- transfer_data$Defensive.Players.Out.Summer[n]
					game_data$Away.Midfield.Players.In.So.Far[j] <- transfer_data$Midfield.Players.In.Summer[n]
					game_data$Away.Midfield.Players.Out.So.Far[j] <- transfer_data$Midfield.Players.Out.Summer[n]
					game_data$Away.Forward.Players.In.So.Far[j] <- transfer_data$Forward.Players.In.Summer[n]
					game_data$Away.Forward.Players.Out.So.Far[j] <- transfer_data$Forward.Players.Out.Summer[n]
					game_data$Away.Prop.Defensive.Players.In.So.Far[j] <- (transfer_data$Defensive.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Midfield.Players.In.So.Far[j] <- (transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Forward.Players.In.So.Far[j] <- (transfer_data$Forward.Players.In.Summer[n] / transfer_data$Total.Players.In.Summer[n]) * 100
					game_data$Away.Prop.Defensive.Players.Out.So.Far[j] <- (transfer_data$Defensive.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
					game_data$Away.Prop.Midfield.Players.Out.So.Far[j] <- (transfer_data$Midfield.Players.In.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
					game_data$Away.Prop.Forward.Players.Out.So.Far[j] <- (transfer_data$Forward.Players.Out.Summer[n] / transfer_data$Total.Players.Out.Summer[n]) * 100
				}
			} else {
			}
		}
	}
	write.csv(game_data, paste(path, "Processed Game Data/", game_name, sep = ""))
	print(sprintf('Completed %s', game_name))
}

for(i in 22:3){
	new_data_name <- paste(years[i-1], years[i-2], ".csv", sep = "")
	new_data <- read.csv(paste(path, "Processed Game Data/", new_data_name, sep = ""), stringsAsFactors = FALSE)

	if(exists("Data")){
		Data <- rbind(Data, new_data)
	} else {
		Data <- new_data
	}
}

write.csv(Data, paste(path, "/Data.csv", sep = ""))
