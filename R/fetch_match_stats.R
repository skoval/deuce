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
	
	output <- cbind(rbind(sapply(values, function(x) x[1])), rbind(sapply(values, function(x) x[1])))
	colnames(output) <- c(paste(sub(".:", "", fields), 1, sep = "."), paste(sub(".:", "", fields), 2, sep = "."))
	output <- as.data.frame(output)
	output$Player1  <- paste(FirstName[1], LastName[1])
	output$Player2 <- paste(FirstName[2], LastName[2])
	output$Time <- MatchTime 

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
		do.call("rbind", lapply(temp_matches, function(x){print(x); extract_match_stats(x)}))
	 }
}