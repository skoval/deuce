#' Download Head-to-Head
#'
#' This function extracts head-to-heads for WTA and ATP players from \url{http://www.stevegtennis.com}
#'
#' @param player1 Character name of player (must match name used by source)
#' @param player2 Character name of player (must match name used by source)
#'
#'@examples
#' fetch_head_to_head("Rafael Nadal", "Roger Federer")
#'
#' @export
#'
#' @return data frame of match activity and results
##' \itemize{
##'  \item{"year"}{Numeric of year of match}
##'  \item{"tournament"}{Character name of tournament}
##'  \item{"round"}{Character of round of match}
##'  \item{"surface"}{Character of surface}
##'  \item{"winner"}{Character name of winner of match}
##'  \item{"loser"}{Character name of loser of match}
##'  \item{"score"}{Character of score}
##'}
fetch_head_to_head <- function(player1, player2){

	player1 <- gsub(" ","_",player1)
	player2 <- gsub(" ","_",player2)
	
	if(type=="ATP")
		base.url <- "http://www.stevegtennis.com/head-to-head/men/player1/player2/"
	else
	    base.url <- "http://www.stevegtennis.com/head-to-head/women/player1/player2/"
	    
	base.url <- sub("player1",player1, base.url)
	base.url <- sub("player2",player2, base.url)
	
	call <- url(base.url)
	lines <- readLines(call, warn = FALSE)
	close(call)
	
	start <- min(grep("matchs_info", lines))
	end <- grep("Last 10 Matches", lines)[1]
	
	lines <- paste(lines[start:end], collapse = "")
	lines <- strsplit(lines, split = "<tr")[[1]]
	
	lines <- strsplit(lines[grep("draw-results", lines)], split = ">")
	lines <- lapply(lines, function(x) x[grepl("^[A-Z]", x) | grepl("^[0-9]" , x)])
	
	do.call("rbind", lapply(lines, function(x){
		x <- sub("(.*)(<.*)", "\\1", x)
		data.frame(
			year = as.numeric(x[1]),
			tournament = x[2],
			round = x[3],
			surface = x[4],
			winner = x[5],
			loser = x[6],
			score = x[7],
			stringsAsFactors = FALSE
		)
	}))
}