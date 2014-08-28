fetch_player_code <- function(player, year = NULL) {
    
    
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    
    if(is.null(year)) year <- 0
    
    call <- collapse("http://www.atpworldtour.com/", call, "?t=pa&y=year&m=s&e=0#")
    call <- sub("year", year, call)
    call <- url(call)
    
    lines <- readLines(call, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    close(call)
   
	get_player_code <- function(lines){
		index <- grep("Match-Facts-Pop-Up", lines)
	sub("(.*p=)([A-Z]...)(.*)","\\2",lines[index])[1]
	}

get_player_code(lines)
}