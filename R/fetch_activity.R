surface <- function(string) {
    if (length(grep("Hard", string)) == 1) 
        "Hard" else if (length(grep("Grass", string)) == 1) 
        "Grass" else "Clay"
}

surface <- Vectorize(surface)

losses <- function(Lines, Start, Stop) length(grep("L&", Lines[Start:Stop]))

wins <- function(Lines, Start, Stop) {
    length(grep("W&", Lines[Start:Stop])) - length(grep("Bye", Lines[Start:Stop]))
}

fetch_activity <- function(player, year = NULL) {
    
    
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    
    if(is.null(year)) year <- 0
    
    call <- collapse("http://www.atpworldtour.com/", call, "?t=pa&y=year&m=s&e=0#")
    call <- sub("year", year, call)
    call <- url(call)
    
    lines <- readLines(call, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    close(call)
    
    if(year == 0)
	    year_pattern <- "\\.[1|2][0-9][0-9][0-9];"
	else
		year_pattern <- paste("\\.", year, ";", sep = "")
		
    tournaments <- grep(year_pattern, lines)
    points <- grep("This Event Points|ATP Ranking", lines)
    
    if ((length(tournaments) != length(points)) & year != 0) {
        year_pattern <- paste(".", as.numeric(year) - 1, ";", sep = "")
        tournaments <- sort(c(tournaments, grep(year_pattern, lines)))
    }
    
    date <- sub("(.*;)([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9])(;.*)", "\\2", lines[tournaments])
    tournament_names <- sub("(.*<strong>)(.*)(</strong>.*)", "\\2", lines[tournaments])
    result <- sub("(.*<td>)(.*)(</td>.*)", "\\2", lines[points - 14])
    
    start <- tournaments
    stop <- points
    
    W <- mapply(wins, Start = start, Stop = stop, MoreArgs = list(Lines = lines))
    L <- mapply(losses, Start = start, Stop = stop, MoreArgs = list(Lines = lines))
    
    the_date <- as.Date(date, "%d.%m.%Y")
    dates <- strsplit(date, split = ".", fixed = TRUE)
    
    day <- as.numeric(sapply(dates, function(x) x[1]))
    month <- as.numeric(sapply(dates, function(x) x[2]))
    year <- as.numeric(sapply(dates, function(x) x[3]))
    
    activity <- data.frame(day = day, month = month, year = year, date = the_date, tournament = tournament_names, result = result, wins = W, 
        losses = L, surface = as.character(surface(lines[tournaments])))
    
    activity
} 
