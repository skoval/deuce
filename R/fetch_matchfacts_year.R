fetch_matchfacts_url <- function(player, year){
	
	url <- fetch_url(player)
	base_url <- "http://www.atpworldtour.com/URL?t=pa&y=YEAR&m=s&e=0#"
	url <- sub("URL", url, base_url)
	url <- sub("YEAR", year, url)
	
	lines <- readLines(url, warn = FALSE)
	on.exit(closeAllConnections())
	
	date_lines <- grep("[0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9]", lines)
	match_lines <- grep("Match-Facts-Pop-Up", lines)
	
	date_lines <- date_lines[max(which(date_lines < min(match_lines))):length(date_lines)]
	date_lines <- date_lines[date_lines < max(match_lines)]
	
	dates <- sub("(.*)([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9])(.*)", "\\2", lines[date_lines])

	tournaments <- sub("(.*)(t=[0-9]+)(&.*)","\\2",lines[match_lines])
	tournaments <- factor(tournaments,  levels = unique(tournaments))
	dates <- dates[tournaments]
	
	urls <- sub("(.*)(/Share/Match-Facts-Pop-Up.aspx.*p=....)(.*)", "\\2", lines[match_lines])
	names(urls) <- dates
	
urls
}


Tags <- c("Tournament",
			"Winner",
			"Player",
			"Time",
			"Aces",
			"Double Faults",
			"1st Serve In",
			"1st Serves",
			"1st Serve Points Won",
			"1st Serve Points",
			"2nd Serve Points Won",
			"2nd Serve In",
			"Break Points Saved",
			"Break Points Faced",
			"Service Games Played",
			"1st Serve Return Points Won","1st Serve Return Points",
			"2nd Serve Return Points Won","2nd Serve Return Points",
			"Break Points Converted","Break Points Opportunities",
			"Return Games Played",
			"Total Service Points Won","Total Service Points",
			"Total Return Points Won","Total Return Points",
			"Total Points Won","Total Points")

is_match <- function(lines){
	
	NotMatch <- length(grep("(/)",lines,fixed=TRUE))>0
	
	if(!NotMatch){
		NoData <- length(grep("N/A Bye",lines,fixed=TRUE))>0
		}
	else{
		NoData <- TRUE
	}

!NoData
}			

check_winner <- function(winner.name, code){
	
	if(length(grep(code,players$code))>0){ # IF PLAYER CODE IN 
		player.name <- players$player[which(players$code==code)]
		return(as.numeric(player.name==winner.name))
	}
	else{
		return(NA)
	}
}


matchfacts_values <- function(lines) {
    
    if (is_match(lines)) {
        
        tournament.index <- grep("Tournament", lines)[1]
        tournament.name <- sub("(.*>)([a-zA-Z].*)(<\\/a.*)", "\\2", lines[tournament.index + 3])
        
        Round <- sub("(.*>)(.*)(<.*)", "\\2", lines[grep("Round", lines) + 3])
        
        # Player Names
        player.name.index <- grep("playerName", lines)
        player.name <- sub("(.*playerName.>)([a-zA-Z].*)(<\\/a.*)", "\\2", lines[player.name.index])
        
        # Get minutes played
        time.index <- grep("minutes", lines)
        time <- sub("(.*>)([0-9]+)(&nbsp.*)", "\\2", lines[time.index])
        
        # firstCells with stat
        index <- grep("firstCell.*>[0-9].*", lines)
        
        # fraction fields
        fraction.index <- grep("[0-9]\\/", lines[index])
        not.fraction.index <- (1:length(index))[is.na(match(1:length(index), fraction.index))]
        
        replacements <- sub("(.*>)([0-9]+)(<.*)", "\\2", lines[index[not.fraction.index]])
        fraction.replacements <- sub("(.*\\()([0-9]+\\/[0-9]+)(\\).*)", "\\2", lines[index[fraction.index]])
  
        opponent.replacements <- sub("(.*>)([0-9]+)(<.*)", "\\2", lines[(index + 1)[not.fraction.index]])
        opponent.fraction.replacements <- sub("(.*\\()([0-9]+\\/[0-9]+)(\\).*)", "\\2", lines[(index + 1)[fraction.index]])
       
        player_stats <- index
        player_stats[fraction.index] <- fraction.replacements
        player_stats[not.fraction.index] <- replacements
        
        opponent_stats <- index
        opponent_stats[fraction.index] <- opponent.fraction.replacements
        opponent_stats[not.fraction.index] <- opponent.replacements
               
        fields <- lapply(player_stats, function(x) strsplit(x, "/")[[1]])
        fields <- as.numeric(unlist(fields))
        fields <- c(tournament.name, 
        			  as.numeric(player.name[1] == player.name[2]), player.name[2], time, fields)

        opponent.fields <- lapply(opponent_stats, function(x) strsplit(x, "/")[[1]])
        opponent.fields <- as.numeric(unlist(opponent.fields))
        opponent.fields <- c(tournament.name, 
        				as.numeric(player.name[1] == player.name[3]), player.name[3], time, opponent.fields)

    } 
    else {
        fields <- rep(NA, length(Tags))
        opponent.fields <- rep(NA, length(Tags))
        Round <- NA
    }
    
    result <- data.frame(
    	V1 = c(as.character(fields[1]), as.character(opponent.fields[1])),
    	V2 = c(as.numeric(fields[2]), as.character(opponent.fields[2])),
    	V3 = c(as.character(fields[3]), as.character(opponent.fields[3])),
    	V4 = c(as.character(fields[4]), as.character(opponent.fields[4])),
    	V5 = c(as.numeric(fields[5]), as.character(opponent.fields[5])),
    	V6 = c(as.numeric(fields[6]), as.character(opponent.fields[6])),
    	V7 = c(as.numeric(fields[7]), as.character(opponent.fields[7])),
    	V8 = c(as.numeric(fields[8]), as.character(opponent.fields[8])),
    	V9 = c(as.numeric(fields[9]), as.character(opponent.fields[9])),
    	V10 = c(as.numeric(fields[10]), as.character(opponent.fields[10])),
    	V11 = c(as.numeric(fields[11]), as.character(opponent.fields[11])),
    	V12 = c(as.numeric(fields[12]), as.character(opponent.fields[12])),
    	V13 = c(as.numeric(fields[13]), as.character(opponent.fields[13])),
    	V14 = c(as.numeric(fields[14]), as.character(opponent.fields[14])),
    	V15 = c(as.numeric(fields[15]), as.character(opponent.fields[15])),
    	V16 = c(as.numeric(fields[16]), as.character(opponent.fields[16])),
    	V17 = c(as.numeric(fields[17]), as.character(opponent.fields[17])),
    	V18 = c(as.numeric(fields[18]), as.character(opponent.fields[18])),
    	V19 = c(as.numeric(fields[19]), as.character(opponent.fields[19])),
    	V20 = c(as.numeric(fields[20]), as.character(opponent.fields[20])),
    	V21 = c(as.numeric(fields[21]), as.character(opponent.fields[21])),
    	V22 = c(as.numeric(fields[22]), as.character(opponent.fields[22])),
    	V23 = c(as.numeric(fields[23]), as.character(opponent.fields[23])),
    	V24 = c(as.numeric(fields[24]), as.character(opponent.fields[24])),
    	V25 = c(as.numeric(fields[25]), as.character(opponent.fields[25])),
    	V26 = c(as.numeric(fields[26]), as.character(opponent.fields[26])),
    	V27 = c(as.numeric(fields[27]), as.character(opponent.fields[27])),
    V28 = c(as.numeric(fields[28]), as.character(opponent.fields[28])),
    	stringsAsFactors = FALSE
    	)
    	
     names(result) <- Tags

     if(length(Round) > 1)
     	result$Round <- NA
     else
     	result$Round <- Round
	 
result     
}

fetch_matchfacts_apply <- function(URLMatchFacts) {

    URLMatchFacts <- url(URLMatchFacts)
    MatchContent <- readLines(con = URLMatchFacts, warn = FALSE)
    on.exit(closeAllConnections())
    
matchfacts_values(MatchContent)
}

fetch_matchfacts <- function(player, year) {
 
 	urls <- fetch_matchfacts_url(player, year)
 	dates <- names(urls)
 	
    urls <- paste("http://www.atpworldtour.com", urls, sep = "")
    drop <- !grepl("r=-", urls)
    urls <- urls[drop] # Remove qualifying/RR
	dates <- dates[drop]  
	
    Result <- do.call("rbind", lapply(urls, fetch_matchfacts_apply))
   	Result$Year <- year
   	Result$Date <- rep(dates, each = 2)
   	Result <- Result[!is.na(Result$Round),]
   	
Result
} 

fetch_matchfacts_from_url <- function(url){
	
	f <- function(url) {

		    	year <- sub("(.*y=)([0-9][0-9][0-9][0-9])(.*)","\\2",url) 
		    on.exit(closeAllConnections())
		    MatchContent <- readLines(con = url, warn = FALSE)
		    result <- matchfacts_values(MatchContent)
			result$Year <- year
	
	result
	}   
	
do.call("rbind", lapply(FUN = f, X = url))
}
