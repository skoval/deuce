fetch_head_to_head <- function(player1, player2, type = "ATP"){

	player1 <- gsub(" ","_",player1)
	player2 <- gsub(" ","_",player2)
	
	if(type=="ATP")
		base.url <- "http://www.stevegtennis.com/head-to-head/men/player1/player2/"
	else
	    base.url <- "http://www.stevegtennis.com/head-to-head/women/player1/player2/"
	    
	base.url <- sub("player1",player1,base.url)
	base.url <- sub("player2",player2,base.url)
	
	call <- url(base.url)
	lines <- readLines(call, warn = FALSE)
	close(call)
	
	end <- grep("Last 10 Matches", lines)[1]
	years <- grep("<td>[0-9][0-9][0-9][0-9]", lines)
	years <- years[years <= end] # only head2head matches
	
	if(type=="ATP")
		winner <- sub("(.*profile-bio/men/)(.*)(/'>.*)","\\2", lines[years])
	else
		winner <- sub("(.*profile-bio/women/)(.*)(/'>.*)","\\2", lines[years])
	years <- sub("(.*<td>)([0-9][0-9][0-9][0-9])(<.*)","\\2", lines[years])
	
data.frame(
	winner = winner,
	years = as.numeric(years)
)
}