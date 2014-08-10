fetch_wta_draw <- function(tournament, year){

	base_url <- "http://www.stevegtennis.com/draw-results/wta/tournament/yyyy/"
	
	tournaments <- c("US_Open","Wimbledon","Australian_Open","French_Open_-%20Roland%20Garros")
	
	if(grepl("US", tournament))
		tournament <- tournaments[1]
	else if(grepl("Wimbledon", tournament))
		tournament <- tournaments[2]		
	else if(grepl("Aus", tournament))
		tournament <- tournaments[3]		
	else if(grepl("French", tournament)|grepl("Roland", tournament))
		tournament <- tournaments[4]		
	else 
		stop("Tournament not found.")
		
	the_url <- sub("tournament",tournament[1],base_url)
	the_url <- sub("yyyy", year, the_url)
	
	con <- url(the_url)
	lines <- tryCatch(readLines(con, warn = FALSE), error = function(x) NA)
	close(con)
	
	if(all(is.na(lines)))
		stop("Draw not found.")
	
	draw <- grep("Winning Player",lines)
	if(length(draw)==0)
		stop("Draw not found.")
	
	draw <- strsplit(lines[draw], split=">")[[1]]
	doubles_index <- grep("Doubles",draw)
	draw <- draw[1:(doubles_index-1)] # Remove doubles results
	
	
	round.index <- lapply(c("^F<","^SF<","^QF<","R16<","R32<","R64<","R128<"), function(x) grep(x, draw))
	
	n.reps <- sapply(round.index, length)
	
	winners <- unlist(sapply(round.index, function(x){
		sub("(.*)(<.*)","\\1",draw[x+3])
	}))
	
	losers <- unlist(sapply(round.index, function(x){
		sub("(.*)(<.*)","\\1",draw[x+7])
	}))
				
	result <- data.frame(
		winner = winners,
		loser = losers,
		round = rep(c(7:1), n.reps),
		date = sub("(.*>)([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9])(<.*)","\\2",lines[grep("Event Details",lines)+2]),
		stringsAsFactors = FALSE
	)
	
	result$date <- ymd(result$date)
	result$year <- year(result$date)
	
result
}