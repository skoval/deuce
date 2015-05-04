fetch_player_code <- function(player, year = NULL) {
    
    
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    
    if(is.null(year)) year <- 0
    
 	base_url <- "http://www.atpworldtour.com/URL?t=pa&y=YEAR&m=s&e=0#"
    url <- sub("URL", call, base_url)
    url <- sub("YEAR", year, url)
 
    result <- readLines(url, warn = FALSE)
    on.exit(closeAllConnections())
    
    x <- grep("p=.*Matchfacts", result)
    
    if(length(x)==0)
    	NA
    else
	    sub("(.*p=)(....)(.*)", "\\2", result[x][1])
 
}