#' Download IBM Slamtracker Point-by-Point Data
#'
#' This function extracts historical point-by-point data for Grand Slam events
#'
#' @param event Character value of slam and one of: 'ausopen', 'wimbledon', 'usopen', or 'frenchopen'
#' @param year Numeric year
#'
#'
#' @export
#' @details This has been tested for 2017. Previous years may not be available as data is archived or site domains have changed.
#' @return data frame of point by point results
##' \itemize{
##'  \item PointServer. Numeric indicator of player who is serving
##'  \item PointWinner. Numeric indicator of player who won the point
##'  \item Set. Numeric of set number
##'  \item Game. Numeric of game number
##'  \item Time. Elapsed time at start of point (hh:mm:ss)
##'  \item Ace. Numeric indicator if server made and ace
##'  \item Winner. Numeric indicator if point ended with a winner
##'  \item DoubleFault. Numeric indicator if point ended in double fault
##'  \item UnforcedError. Numeric indicator if point ended in an unforced error
##'  \item NetPoint. Numeric indicator if point was a net point
##'  \item WinnerShotType. Character of type of shot for winner (F or B)
##'  \item ServerError. Logical if server made an error
##'  \item ReceiverError. Logical if receiver made an error 
##'  \item SpeedMPH. Numeric of MPH of serve
##'  \item ServeNumber. Numeric indicator of first or second serve
##'  \item BreakPointWon. Numeric indicator if break point was won
##'  \item BreakPointOpportunity. Numeric indicator if had a break point opportunity
##'  \item GameWinner. Numeric if point ended in game winner and for which player
##'  \item ForcedError. Numeric of whether point ended in forced error
##'  \item MatchCode. Identifier of the match
##'  \item Round. Numeric value of round (1 = First, 7 = Final)
##'  \item ServerName. Character of server's name
##'  \item Player1. Character of Player 1 name
##'  \item Player2. Character of PLayer 2 name
##'}
fetch_slam_pbp <- function(event, year){
	
	warn <- options("warn")[[1]]
	options("warn" = -1)
	on.exit(options("warn" = warn))

	f <- function(url, day = 1){
		
		id_pattern <- "scores/stats/[1-2][0-9]+ms.html"
		
		file <- sub("NUM", day, url)
	
		lines <- tryCatch(readLines(file), error = function(x) NULL)
		
		if(!is.null(lines)){
	
			x <- grep(id_pattern, lines, val = TRUE)
			
			if(length(x) == 1)
				x <- strsplit(x, "<")[[1]]
				
			ids <- sub("(.*/)([0-9]+)(ms.html.*)", "\\2", grep(id_pattern, x, val = TRUE))
			
			tourney <- ifelse(grepl("ausopen", url), "ausopen",
					ifelse(grepl("roland", url), "frenchopen",
						ifelse(grepl("wimbledon", url), "wimbledon", "usopen")))
						
			result <- data.frame(
				event = tourney,
				matchid = ids,
				maindraw = nchar(ids) == 4
			)
			
		result 	%>% filter(maindraw)
		}
		else{
			NULL
		}
	}
	

	
	read_json <- function(matchid, url){
		
		code <- function(x){
			the_code <- sub("(.*/)([0-9]+)(C\\.json)", "\\2", x)
			match.type <- ifelse(substr(the_code, 1, 1) == "1", "MS", "WS")
			
		paste(match.type, substr(the_code, 2, 4), sep = "")
		}
		
		file <- paste(url, matchid, "C.json", sep = "")
		
		data <- fromJSON(file, flatten=TRUE)
	
		keys <- fromJSON(sub("history", "keys", sub("C", "keys", file)), flatten = FALSE)
		
		keys[[1]]$key <- NULL
		keys <- as.data.frame(keys[[1]])[,1:2]
		names(keys) <- c("PointServer", "ServerName")
		
		data <- data %>%
			dplyr::mutate(
				PointWinner = as.numeric(PointWinner),
				PointServer = as.numeric(PointServer),
				Set = as.numeric(SetNo),
				Game = as.numeric(GameNo),
				Time = as.character(ElapsedTime),
				PointNumber = as.numeric(PointNumber),
				Ace =  as.numeric(Ace),
				Winner = as.numeric(Winner),
				DoubleFault = as.numeric(DoubleFault),
				UnforcedError = as.numeric(UnforcedError),
				NetPoint = as.numeric(NetPoint),
				RallyCount = as.numeric(RallyCount),
				WinnerShotType = as.character(WinnerShotType),
				ServerError = (PointServer != PointWinner) & ifelse(PointServer == 1, 2, 1) != Winner,
				ReceiverError = (PointServer != Winner) & ifelse(PointServer == 1, 2, 1) != PointWinner,
				ForcedError = ifelse(ServerError & UnforcedError != PointServer, PointServer,
					ifelse(ReceiverError & UnforcedError != ifelse(PointServer == 1, 2, 1), 
						ifelse(PointServer == 1, 2, 1), 0))
				
			)  %>%
			select(PointWinner, PointServer, Set, Game, Time, PointNumber, Ace, Winner, DoubleFault, UnforcedError, NetPoint, RallyCount, WinnerShotType, ServerError, ReceiverError, ForcedError, Speed_MPH, ServeNumber, BreakPointWon, BreakPointOpportunity, GameWinner)              
		
		data$MatchCode <- code(file)
		data$Round <- as.numeric(substr(data$MatchCode, 3, 3))
	
		data$RallyCount[data$RallyCount == 0] <- 1
		
		data$RallyCount <- data$RallyCount  + ifelse((data$RallyCount %% 2 == 0 & (data$Winner == data$PointServer | data$ServerError)) | 
		(data$RallyCount %% 2 == 1 & (data$Winner == ifelse(data$PointServer == 1, 2, 1) | data$ReceiverError)), 1, 0)
		
		data$RallyCount[data$RallyCount == 0 & (data$PointServer == data$PointWinner | data$ServerError)] <- 1
		
		data$RallyCount[data$RallyCount == 0 & (data$PointServer == data$PointWinner | data$ReceiverError)] <- 2
		
		data <- merge(data, keys, by = "PointServer", all.x = T)
		
		data$Player1 <- keys$ServerName[1]
		data$Player2 <- keys$ServerName[2]
		
		data <- data %>% filter(!is.na(ServerName))
	data	
	}

	if(event == "ausopen"){
		id_url <- "http://YEAR.ausopen.com/en_AU/scores/completed_matches/dayNUM.html"
		
		base_pbp <- "http://YEAR.ausopen.com/en_AU/xml/gen/slamtracker/history/"
	}
	else if(event == "wimbledon"){
		
		id_url <- "http://YEAR.wimbledon.com/en_GB/scores/results/dayNUM.html"
		
		base_pbp <- "http://YEAR.wimbledon.com/en_GB/xml/gen/slamtracker/history/"
	}
	else if(event == "usopen"){
		id_url <- "http://YEAR.usopen.org/en_US/scores/completed_matches/dayNUM.html"
		
		base_pbp <- "http://YEAR.usopen.org/en_US/xml/gen/slamtracker/history/"
	}
	else{
		
		id_url <- "http://YEAR.rolandgarros.com/en_FR/scores/completed_matches/dayNUM.html"
		
		base_pbp <- "http://YEAR.rolandgarros.com/en_FR/xml/gen/slamtracker/history/"
	}

	# Extract IDS & pull point-by-point
	id_url <- sub("YEAR", year, id_url)
	base_pbp <- sub("YEAR", year, base_pbp)
	
	get_ids <- function(x) tryCatch(f(x, url = id_url), error = function(x) NULL)
	
	matches <- do.call("rbind", lapply(1:21, get_ids))
	
	if(is.null(matches) | nrow(matches) == 0)
		stop("No matches found for this year and event.")

do.call("rbind", lapply(matches$matchid, read_json, url = base_pbp))
}