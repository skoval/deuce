#' Tidy Point-by-Point Data from Source
#' 
#' @export
tidy_point_by_point <- function(data){
	
	add_semicolon <- function(xstr){

	print(xstr)
	
	points <- strsplit(xstr, split = "")[[1]]
   	ace <- grepl("A", points)
	df <- grepl("D", points)
	points <- sub("D", "R", sub("A", "S",  points))
	x <- points == "S"
	xpoints <- points
	
	semicolon <- NULL

	for(i in 4:length(x)){
		# All possible end game scenarios
		if((sum(x[1:i], na.rm = TRUE) >= 4 & sum((1-x)[1:i], na.rm = TRUE) < 3) |
		    (sum((1-x)[1:i], na.rm = TRUE) >= 4 & sum(x[1:i], na.rm = TRUE) < 3) |
		    (sum(x[1:i], na.rm = TRUE) >= 4 & sum((1-x)[1:i], na.rm = TRUE) >= 3 &
		    (sum(x[1:i], na.rm = TRUE) - sum((1-x)[1:i], na.rm = TRUE)) >= 2 |
		     (sum((1-x)[1:i], na.rm = TRUE) >= 4 & sum(x[1:i], na.rm = TRUE) >= 3 &
		       (sum(x[1:i], na.rm = TRUE) - sum((1-x)[1:i], na.rm = TRUE)) <= -2))) {
			semicolon <- c(semicolon, i)
			x[1:i] <- NA
		}	
	}

	start <- c(1, semicolon[-length(semicolon)] + 1)
	end <- semicolon
	
	processed <- mapply(function(source, x, y) substr(source, x, y), 
		x = start, y = end, MoreArgs = list(source = xstr))

	paste(unlist(processed), sep = "", collapse = ";")
	}			

	add_semicolon <- Vectorize(add_semicolon)


	data$tny_date <- dmy(data$date)

	# Parse scores 
	games_won <- sapply(strsplit(data$score, split = " "), function(x){
		strsplit(gsub("\\([0-9]+\\)","",gsub("[A-Za-z]","",x)), split = "-")
		})
	
	winner_games <- sapply(games_won, function(x) {
		result <- sapply(x, function(z) z[1])
		if(all(is.na(result)))
			rep(NA, 5)
		else
			c(result[!is.na(result)], rep(NA, 5 - length(result[!is.na(result)])))
	})
	
	loser_games <- sapply(games_won, function(x) {
		result <- sapply(x, function(z) z[2])
		if(all(is.na(result)))
			rep(NA, 5)
		else
			c(result[!is.na(result)], rep(NA, 5 - length(result[!is.na(result)])))
	})
	
	data[,c("W1", "W2", "W3", "W4", "W5")] <- t(winner_games)
	data[,c("L1", "L2", "L3", "L4", "L5")] <- t(loser_games)
	
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
	
	sets <- strsplit(data$pbp, split = "\\.")
	
	tiebreaks <- sapply(sets, function(x){
		result <- sapply(x, function(z) {
			if(grepl("/", z)){
				z <- strsplit(z, split = ";")[[1]]
			z[length(z)]	
			}
			else
				NA
		})
	c(result, rep(NA, 5 - length(result)))
	})
	
	
	sets <- sapply(sets, function(x){
		result <- sapply(x, function(z) {
			if(grepl("/", z)){
				z <- strsplit(z, split = ";")[[1]]
			paste(z[-length(z)], collapse = "")
			}
			else
				z
		})
	c(result, rep(NA, 5 - length(result)))
	})
	
	data[,c("Set1","Set2","Set3","Set4","Set5")] <- t(sets)
	data[,c("TB1","TB2","TB3","TB4","TB5")] <- t(tiebreaks)
	data$pbp <- NULL
	
	data$Set1[data$Set1 == ""] <- NA
	data$Set2[data$Set2 == ""] <- NA
	data$Set3[data$Set3 == ""] <- NA
	data$Set4[data$Set4 == ""] <- NA
	data$Set5[data$Set5 == ""] <- NA
	
	for(i in paste("Set", 1:5, sep = "")){
	
		if(any(!grepl(";", data[,i]) & !is.na(data[,i]))){
			cases <- !grepl(";", data[,i]) & !is.na(data[,i])
			data[cases,i] <- add_semicolon(data[cases, i])
		}
		
	}
	
	
	data$pbp_id <- NULL
	data$wh_minutes <- NULL

data
}