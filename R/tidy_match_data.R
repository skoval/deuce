#' @export
tidy_match_data <- function(data, atp = T){
		
	# Parse scores 
	data$score[data$score == "2-6 6 2 7-9 6-4 6-3"] <- "2-6 6-2 7-9 6-4 6-3"
	data$score[data$score == "3-6 6 -3 6-8 6-2 10-8"] <- "3-6 6-3 6-8 6-2 10-8"
	data$score[data$score == "6-3 1-6 4-6 1-0 8 6-0"] <- "6-3 1-6 4-6 1-6 6-0"

	games_won <- sapply(strsplit(data$score, split = " "), function(x){
		strsplit(gsub("\\([0-9]+\\)","",gsub("[A-Za-z]","",x)), split = "-")
		})
	
	winner_games <- sapply(games_won, function(x) {
		result <- sapply(x, function(z) ifelse(grepl("[[:punct:]]", z[1]), NA, z[1]))
		if(all(is.na(result)))
			rep(NA, 5)
		else{
			result <- c(result[!is.na(result)], rep(NA, 5 - min(c(5, length(result[!is.na(result)])))))
			result <- as.numeric(result)
			result
		}
	})
	
	loser_games <- sapply(games_won, function(x) {
		result <- sapply(x, function(z) z[2])
		if(all(is.na(result)))
			rep(NA, 5)
		else{
			result <- c(result[!is.na(result)], rep(NA, 5 - min(c(5, length(result[!is.na(result)])))))
			result <- as.numeric(result)
			result
		}
	})
	
	winner_games <- t(winner_games)
	winner_games <- apply(winner_games, 2, as.numeric)
	
	loser_games <- t(loser_games)
	loser_games <- apply(loser_games, 2, as.numeric)
	
	data[,c("W1", "W2", "W3", "W4", "W5")] <- winner_games
	data[,c("L1", "L2", "L3", "L4", "L5")] <- loser_games
	
	# Retirement and other
	data$Retirement <- grepl("[A-Za-z]", data$score)
	
	tiebreak_won <- sapply(strsplit(data$score, split = " "), function(x){
		result <- sub("(.*\\()([0-9]+)(\\).*)","\\2",gsub("[A-Za-z]","", x))
		result[!grepl("\\([0-9]", x)] <- NA
		final_result <- rep(NA, 5)
		final_result[which(!is.na(result) & result != "")] <- result[!is.na(result) & result != ""]
	final_result
		})
	
	data[,c("T1", "T2", "T3", "T4", "T5")] <- t(tiebreak_won)
	
	W <- c("W1", "W2", "W3", "W4", "W5")
	L <- c("L1", "L2", "L3", "L4", "L5")
	TB <-  c("T1", "T2", "T3", "T4", "T5")
	WTB <- c("WTB1", "WTB2", "WTB3", "WTB4", "WTB5")
	LTB <- c("LTB1", "LTB2", "LTB3", "LTB4", "LTB5")
	
	for(j in TB) data[,j] <- as.numeric(data[,j])
	
	
	for(i in 1:5){
	
	data[,WTB[i]] <- ifelse(!is.na(data[,TB[i]]) & data[,W[i]] == 7, 
				ifelse(data[,TB[i]] >= 5, data[,TB[i]] + 2, 7),
				ifelse(!is.na(data[,TB[i]]) & data[,W[i]] != 7, data[,TB[i]], NA))
	
	data[,LTB[i]] <- ifelse(!is.na(data[,TB[i]]) & data[,L[i]] == 7, 
				ifelse(data[,TB[i]] >= 5, data[,TB[i]] + 2, 7),
				ifelse(!is.na(data[,TB[i]]) & data[,L[i]] != 7, data[,TB[i]], NA))
	
	}
	
	for(i in TB) data[,i] <- NULL
	
	data$surface[data$surface == ""] <- NA
	
	if(atp){
		
		data$tourney_level <- factor(data$tourney_level, 
			levels = c("A", "C", "D", "F", "G", "M", "Q", "S"),
			labels = c("250 or 500","Challenger","Davis Cup","Tour Finals","Grand Slams", "Masters", "Qualifying", "Futures"))
			
		data$tourney_level[data$tourney_name == "Monte-Carlo"] <- "Masters"
		data$tourney_name[data$tourney_name == "Rio de Janiero"] <- "Rio de Janeiro"	
		
		data$winner_name[data$winner_name == "Joao Souza"] <- "Joao Sousa"
		
		data$loser_name[data$loser_name == "Joao Souza"] <- "Joao Sousa"
		
		data$winner_name[data$winner_name == "Stanislas Wawrinka"] <- "Stan Wawrinka"
		
		data$loser_name[data$loser_name == "Stanislas Wawrinka"] <- "Stan Wawrinka"
	}
	else{
		
		data$tourney_level <- factor(data$tourney_level, 
	levels = c("10", "100", "125", "20", "25", "50", "75", "CC", "CH", "D", 
"G", "I", "O", "P", "PM", "T1", "T2", "T3", "T4", "T5", "W"),
	labels = c("C10",
				"C100",
				"C125",
				"C20",
				"C25",
				"C50",
				"C75",
					"Satellite",
					"Challenger",
					"Davis Cup",
					"Grand Slams",
					"International",
					"Olympics",
					"Premier",
					"Premier Mandatory",
					"Tier I",
					"Tier II",
					"Tier III",
					"Tier IV",
					"Tier V",
					"Historical"))		
	}
	
	data$tourney_start_date <- ymd(data$tourney_date)
	
	data$tourney_date <- NULL
	
	data$year <- year(data$tourney_start_date)
	
	data$round <- factor(data$round, level = c("BR", "Q1", "Q2", "Q3", "Q4",  "R1", "R128", "R62", "R63","R64", "R3", "R28", "R29","R31", "R32", "R15", "R16", "QF", "RR", "SF", "R7", "F"), order = T)
	
	data <- data[order(data$tourney_start_date, data$tourney_name, data$round),]
	
	data <- data %>%
		group_by(tourney_start_date, tourney_name) %>%
		dplyr::mutate(
			match_num = 1:n()
		)
	
	data <- as.data.frame(data)
	
	data$match_id <- paste(data$tourney_id, data$match_num, sep = ":")

data
}