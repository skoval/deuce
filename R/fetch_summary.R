make_or_string <- function(x) {
    paste(paste("(", x, ")", sep = ""), collapse = "|")
}

service_stat_fields <- c("Aces", "Double Faults", "1st Serve", "1st Serve Points Won", "2nd Serve Points Won", "Break Points Faced", 
    "Break Points Saved", "Service Games Played", "Service Games Won", "Service Points Won")

return_stat_fields <- c("1st Serve Return Points Won", "2nd Serve Return Points Won", "Break Points Opportunities", "Break Points Converted", 
    "Return Games Played", "Return Games Won", "Return Points Won", "Total Points Won")

or_string <- make_or_string(c(service_stat_fields, return_stat_fields))


fetch_summary <- function(players, year = NULL, surface = c("All", "Clay", "Hard", "Grass")) {
    
    demo = FALSE
      
    if(is.null(year)) year <- 0
    
    SurfaceType <- function(type) {
        
        if (type == "All") 
            0 else if (type == "Clay") 
            1 else if (type == "Hard") 
            3 else 2
    }

    FieldIndex <- function(lines) {
        grep(collapse(">(", or_string, ")</li>"), lines)
    }
       
    get_stats <- function(lines, index) {
        
        if (is.na(lines[1]) || length(index) == 0) {
            Values <- rep(NA, length(c(service_stat_fields, return_stat_fields)))
            names(Values) <- c(service_stat_fields, return_stat_fields)
        } else {
            
            Values <- lines[index]
            Values <- sub("(.*<span>)(.*[0-9])(%?</span>.*)", "\\2", Values)
            Values <- as.numeric(sub(",", "", Values))
            names(Values) <- c(service_stat_fields, return_stat_fields)
        }
        
        Values
    }
    
    
    fetch_summary <- function(player, year = NULL, surface = c("All", "Clay", "Hard", "Grass")) {
        
        call <- fetch_url(player)
        if (is.na(call)) 
            stop("Player not found.")
        call <- collapse("http://www.atpworldtour.com/", call, "?t=mf&y=year&s=surface#")
        call <- sub("year", year, call)
        call <- sub("surface", SurfaceType(surface), call)

        call <- url(call)
        
        lines <- readLines(call, ok = TRUE, warn = FALSE, n = 400, encoding = "UTF-8")
        on.exit(closeAllConnections())
        
        get_stats(lines, FieldIndex(lines))
    }
    
    
    if (missing(surface)) 
    surface <- "All"
    warn <- getOption("warn")
    options(warn = -1)  # SUPPRESS WARNINGS DURING FUNCTION CALL
    fetch_atp <- ldply(players, function(name) fetch_summary(name, year = year, surface = surface))
    # CONVERT TO NUMERIC
    fetch_atp$Aces <- as.numeric(fetch_atp$Aces)
    fetch_atp$"Double Faults" <- as.numeric(fetch_atp$"Double Faults")
    fetch_atp$"1st Serve" <- as.numeric(fetch_atp$"1st Serve")
    fetch_atp$"1st Serve Points Won" <- as.numeric(fetch_atp$"1st Serve Points Won")
    fetch_atp$"2nd Serve Points Won" <- as.numeric(fetch_atp$"2nd Serve Points Won")
    fetch_atp$"Break Points Faced" <- as.numeric(fetch_atp$"Break Points Faced")
    fetch_atp$"Break Points Saved" <- as.numeric(fetch_atp$"Break Points Saved")
    fetch_atp$"Service Games Played" <- as.numeric(fetch_atp$"Service Games Played")
    fetch_atp$"Service Games Won" <- as.numeric(fetch_atp$"Service Games Won")
    fetch_atp$"Service Points Won" <- as.numeric(fetch_atp$"Service Points Won")
    fetch_atp$"1st Serve Return Points Won" <- as.numeric(fetch_atp$"1st Serve Return Points Won")
    fetch_atp$"2nd Serve Return Points Won" <- as.numeric(fetch_atp$"2nd Serve Return Points Won")
    fetch_atp$"Break Points Opportunities" <- as.numeric(fetch_atp$"Break Points Opportunities")
    fetch_atp$"Break Points Converted" <- as.numeric(fetch_atp$"Break Points Converted")
    fetch_atp$"Return Games Played" <- as.numeric(fetch_atp$"Return Games Played")
    fetch_atp$"Return Games Won" <- as.numeric(fetch_atp$"Return Games Won")
    fetch_atp$"Return Points Won" <- as.numeric(fetch_atp$"Return Points Won")
    fetch_atp$"Total Points Won" <- as.numeric(fetch_atp$"Total Points Won")
    
    fetch_atp$player <- players
    options(warn = warn)
    
    fetch_atp
} 
