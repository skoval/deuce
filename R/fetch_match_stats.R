#' Download Match Statistics
#'
#' This function extracts statistics for each player for all ATP, Grand Slam and Olympic events
#'
#' @param player Character name of ATP player
#' @param year Numeric year or "all" for all years
#'
#'@examples
#' \dontrun{
#' fetch_match_stats("Alexander Zverev", 2017)
#'}
#'
#' @export
#'
#' @return data frame of match activity and results
##' \itemize{  
##'  \item Aces.1. Count of aces for player 1                             
##'  \item AcesPercentage.1. Percentage of aces out of total aces in match                       
##'  \item DoubleFaults.1. Count of double faults for player 1                       
##'  \item DoubleFaultsPercentage.1. Percentage of double faults in match             
##'  \item FirstServePercentage.1. First serve percentage for player 1               
##'  \item FirstServeDividend.1. Numerator for first serve percentage for player 1                 
##'  \item FirstServeDivisor.1. Denominator for first serve percentage for player 1                  
##'  \item FirstServePointsWonPercentage.1. Percentage of first serve points won for player 1      
##'  \item FirstServePointsWonDividend.1. Numerator for first serve points won for player 1        
##'  \item FirstServePointsWonDivisor.1. Denominator for first serve points won for player 1         
##'  \item SecondServePointsWonPercentage.1. Percentage of second serve points won for player 1     
##'  \item SecondServePointsWonDividend.1. Numerator for second serve points won for player 1       
##'  \item SecondServePointsWonDivisor.1. Denominator for second serve points won for player 1        
##'  \item BreakPointsSavedPercentage.1. Break points saved percentage for player 1         
##'  \item BreakPointsSavedDividend.1. Numerator for break points saved percentage           
##'  \item BreakPointsSavedDivisor.1. Denominator for break points saved percentage            
##'  \item ServiceGamesPlayed.1. Count of service games played for player 1                 
##'  \item ServiceGamesPlayedPercentage.1. Percentage of service games played out of match       
##'  \item FirstServeReturnPointsPercentage.1. Percentage of first serve return for player 1   
##'  \item FirstServeReturnPointsDividend.1. Numerator of first serve return percentage     
##'  \item FirstServeReturnPointsDivisor.1. Denominator of first serve return percentage      
##'  \item SecondServePointsPercentage.1. Percentage of second serve for player 1        
##'  \item SecondServePointsDividend.1. Numerator of second serve percentage          
##'  \item SecondServePointsDivisor.1. Denominator of second serve percentage           
##'  \item BreakPointsConvertedPercentage.1. Percentage of break points converted for player 1     
##'  \item BreakPointsConvertedDividend.1. Numerator of break points converted       
##'  \item BreakPointsConvertedDivisor.1. Denominator of break points converted        
##'  \item ReturnGamesPlayed.1. Total of return games played for player 1                  
##'  \item ReturnGamesPlayedPercentage.1. Percentage of return games played out of match        
##'  \item TotalServicePointsWonPercentage.1. Percentage of total service points won for player 1    
##'  \item TotalServicePointsWonDividend.1. Numerator for total service points won for player 1      
##'  \item TotalServicePointsWonDivisor.1. Denominator for total service points won for player 1       
##'  \item TotalReturnPointsWonPercentage.1. Percentage of total return points won for player 1     
##'  \item TotalReturnPointsWonDividend.1. Numerator of return points won       
##'  \item TotalReturnPointsWonDivisor.1. Denominator of return points won        
##'  \item TotalPointsWonPercentage.1. Percentage of total points won by player 1           
##'  \item TotalPointsWonDividend.1. Numerator for total points won             
##'  \item TotalPointsWonDivisor.1. Denominator for total points won              
##'  \item Aces.2. Count of aces for player 2                             
##'  \item AcesPercentage.2. Percentage of aces out of total aces in match                       
##'  \item DoubleFaults.2. Count of double faults for player 2                       
##'  \item DoubleFaultsPercentage.2. Percentage of double faults in match             
##'  \item FirstServePercentage.2. First serve percentage for player 2               
##'  \item FirstServeDividend.2. Numerator for first serve percentage for player 2                 
##'  \item FirstServeDivisor.2. Denominator for first serve percentage for player 2                  
##'  \item FirstServePointsWonPercentage.2. Percentage of first serve points won for player 2      
##'  \item FirstServePointsWonDividend.2. Numerator for first serve points won for player 2        
##'  \item FirstServePointsWonDivisor.2. Denominator for first serve points won for player 2         
##'  \item SecondServePointsWonPercentage.2. Percentage of second serve points won for player 2     
##'  \item SecondServePointsWonDividend.2. Numerator for second serve points won for player 2       
##'  \item SecondServePointsWonDivisor.2. Denominator for second serve points won for player 2        
##'  \item BreakPointsSavedPercentage.2. Break points saved percentage for player 2         
##'  \item BreakPointsSavedDividend.2. Numerator for break points saved percentage           
##'  \item BreakPointsSavedDivisor.2. Denominator for break points saved percentage            
##'  \item ServiceGamesPlayed.2. Count of service games played for player 2                 
##'  \item ServiceGamesPlayedPercentage.2. Percentage of service games played out of match       
##'  \item FirstServeReturnPointsPercentage.2. Percentage of first serve return for player 2   
##'  \item FirstServeReturnPointsDividend.2. Numerator of first serve return percentage     
##'  \item FirstServeReturnPointsDivisor.2. Denominator of first serve return percentage      
##'  \item SecondServePointsPercentage.2. Percentage of second serve for player 2        
##'  \item SecondServePointsDividend.2. Numerator of second serve percentage          
##'  \item SecondServePointsDivisor.2. Denominator of second serve percentage           
##'  \item BreakPointsConvertedPercentage.2. Percentage of break points converted for player 2     
##'  \item BreakPointsConvertedDividend.2. Numerator of break points converted       
##'  \item BreakPointsConvertedDivisor.2. Denominator of break points converted        
##'  \item ReturnGamesPlayed.2. Total of return games played for player 2                  
##'  \item ReturnGamesPlayedPercentage.2. Percentage of return games played out of match        
##'  \item TotalServicePointsWonPercentage.2. Percentage of total service points won for player 2    
##'  \item TotalServicePointsWonDividend.2. Numerator for total service points won for player 2      
##'  \item TotalServicePointsWonDivisor.2. Denominator for total service points won for player 2       
##'  \item TotalReturnPointsWonPercentage.2. Percentage of total return points won for player 2     
##'  \item TotalReturnPointsWonDividend.2. Numerator of return points won       
##'  \item TotalReturnPointsWonDivisor.2. Denominator of return points won        
##'  \item TotalPointsWonPercentage.2. Percentage of total points won by player 2           
##'  \item TotalPointsWonDividend.2. Numerator for total points won             
##'  \item TotalPointsWonDivisor.2. Denominator for total points won   
##'  \item Player1. Character name of player 1                              
##'  \item Player2. Character name of player 2                              
##'  \item Time. Character match time HH:MM:SS                                 
##'  \item Tournament. Character name of tournament                           
##'  \item Round. Character name of round                                
##'  \item Year. Numeric of year  
##'}
##'
fetch_match_stats <- function(player, year){
	
extract_match_stats <- function(file){
	
	lines <- readLines(file)
	
	MatchTime <- grep("Time.:", lines, val = TRUE)[1]
	MatchTime <- sub("(.*)([0-9][0-9]:[0-9][0-9]:[0-9][0-9])(.*)", "\\2", MatchTime)
	
	FirstName <- lines[grep("first-name", lines) + 1]
	LastName <- lines[grep("last-name", lines) + 1]
	
	FirstName <- gsub("\t", "", FirstName)
	LastName <- gsub("\t", "", LastName)
	
	fields <- c(
		"Aces.:",
		"AcesPercentage.:",
		"DoubleFaults.:",
		"DoubleFaultsPercentage.:",
		"FirstServePercentage.:",
		"FirstServeDividend.:",
		"FirstServeDivisor.:",
		"FirstServePointsWonPercentage.:",
		"FirstServePointsWonDividend.:",
		"FirstServePointsWonDivisor.:",
		"SecondServePointsWonPercentage.:",
		"SecondServePointsWonDividend.:",
		"SecondServePointsWonDivisor.:",
		"BreakPointsSavedPercentage.:",
		"BreakPointsSavedDividend.:",
		"BreakPointsSavedDivisor.:",
		"ServiceGamesPlayed.:",
		"ServiceGamesPlayedPercentage.:",
		"FirstServeReturnPointsPercentage.:",
		"FirstServeReturnPointsDividend.:",
		"FirstServeReturnPointsDivisor.:",
		"SecondServePointsPercentage.:",
		"SecondServePointsDividend.:",
		"SecondServePointsDivisor.:",
		"BreakPointsConvertedPercentage.:",
		"BreakPointsConvertedDividend.:",
		"BreakPointsConvertedDivisor.:",
		"ReturnGamesPlayed.:",
		"ReturnGamesPlayedPercentage.:",
		"TotalServicePointsWonPercentage.:",
		"TotalServicePointsWonDividend.:",
		"TotalServicePointsWonDivisor.:",
		"TotalReturnPointsWonPercentage.:",
		"TotalReturnPointsWonDividend.:",
		"TotalReturnPointsWonDivisor.:",
		"TotalPointsWonPercentage.:",
		"TotalPointsWonDividend.:",
		"TotalPointsWonDivisor")
	
	
	get_field <- function(x) as.numeric(sub('(.*:)( ?.*[0-9])(,?.*)', "\\2", x))

	stats_data <- lapply(fields, function(x) grep(x, lines, val = TRUE)[1:2])
	
	values <- lapply(stats_data, get_field)
	
	output <- cbind(rbind(sapply(values, function(x) x[1])), rbind(sapply(values, function(x) x[2])))
	
	colnames(output) <- c(paste(sub(".:", "", fields), 1, sep = "."), paste(sub(".:", "", fields), 2, sep = "."))
	
	output <- as.data.frame(output)
	
	output$Player1  <- paste(FirstName[1], LastName[1])
	output$Player2 <- paste(FirstName[2], LastName[2])
		
	Time <- lines[grep("^Time:", lines)]
	Time <- sub("(Time.*)([0-9][0-9]:[0-9][0-9]:[0-9][0-9])(.*)", "\\2", Time)
		
	output$Time <- Time
		
	tournament <- sub("(.*en/tournaments/)(.*)(/[0-9].*)", "\\2", grep("en/tournaments/.*overview", lines, val = T))
	
	if(length(tournament) == 0)
		tournament <- gsub("\t", "", grep("Olympic Games|Davis Cup", lines, val = T))
		
	Round_Pattern <- "Olympic|Round of|Round Robin|Final|Round Qualifying"
		
	Round <- gsub("\t", "", lines[max(grep(Round_Pattern, lines))])
		
	output$Tournament <- tournament
	output$Round <- Round
		
output
}

	warn.source <- options("warn")$warn
	on.exit(options(warn = warn.source))
	options(warn = -1)
	
    data(atp_player_sites)
    
    site <- atp_player_sites$site[atp_player_sites$player == player]
    site <- paste("http://www.atpworldtour.com/", site, sep = "")
    site <- sub("overview", "player-activity?year=YEAR", site)
    site <- sub("YEAR", year, site)
	
    lines <- readLines(site)
	
	any_match_stat <- grep("match-stats", lines)
	
	if(length(any_match_stat) == 0)
		return(NULL)
	else{
		
		temp_matches <- sub("(.*)(/en.*match-stats)(.*)", "http://www.atpworldtour.com\\2", grep("match-stats", lines, val = TRUE))
		
		result <- do.call("rbind", lapply(temp_matches, function(x) extract_match_stats(x)))
	
		
		 result$Year <- year
	 
	 result
	 }
}