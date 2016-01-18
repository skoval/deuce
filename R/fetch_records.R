matchrecord_pattern <- "td class.*([a-zA-Z]|\\*|(Masters 1000)|(Set.)|(Top 10))<.td"


match_fields <- c("Overall", "Grand Slams", "Masters 1000", "Tiebreaks", "Versus Top 10", "Finals", "Deciding Set", "5th Set", "Hard", 
    "Clay", "Grass", "Carpet", "Indoor", "Outdoor", "After 1st Set Win", "After 1st Set Loss", "vs. Righties", "vs. Lefties")

match_fields <- c(paste(match_fields, "YTD Won"), paste(match_fields, "YTD Lost"), paste(match_fields, "Career Won"), paste(match_fields, 
    "Career Lost"))


match_index <- function(lines) {
    i <- grep(matchrecord_pattern, lines)
    if (length(i) == 0) {
        NA
    } else {
        i <- i[c(1:3, 5:9, 11:16, 18:21)]
        i <- c(i + 1, i + 2, i + 6, i + 7)
    }
}


get_matchstats <- function(lines, index) {
    
    if (is.na(lines[1]) || length(index) == 0) {
        Values <- rep(NA, length(match_fields))
    } else {
        Values <- sub("(.*>)([0-9]+)(<.*)", "\\2", lines[index])
        Values <- as.numeric(Values)
    }
    names(Values) <- match_fields
    Values
}

fetch_records <- function(players) {
    
    
    atp_stats <- function(player) {
        
        call <- fetch_url(player)
        
        if (is.na(call)) 
            stop("Player not found") else call <- url(collapse("http://www.atpworldtour.com/", call, "?t=mr"))
        source <- readLines(call, ok = TRUE, warn = FALSE, n = 800, encoding = "UTF-8")
        close(call)
        get_matchstats(source, match_index(source))
    }
    
    warn <- getOption("warn")
    options(warn = -1)  # SUPPRESS WARNINGS DURING FUNCTION CALL
    fetch_atp <- ldply(players, function(name) atp_stats(name))
    fetch_atp$"Overall YTD Won" <- as.numeric(fetch_atp$"Overall YTD Won")
    fetch_atp$"Grand Slams YTD Won" <- as.numeric(fetch_atp$"Grand Slams YTD Won")
    fetch_atp$"Masters 1000 YTD Won" <- as.numeric(fetch_atp$"Masters 1000 YTD Won")
    fetch_atp$"Tiebreaks YTD Won" <- as.numeric(fetch_atp$"Tiebreaks YTD Won")
    fetch_atp$"Versus Top 10 YTD Won" <- as.numeric(fetch_atp$"Versus Top 10 YTD Won")
    fetch_atp$"Finals YTD Won" <- as.numeric(fetch_atp$"Finals YTD Won")
    fetch_atp$"Deciding Set YTD Won" <- as.numeric(fetch_atp$"Deciding Set YTD Won")
    fetch_atp$"5th Set YTD Won" <- as.numeric(fetch_atp$"5th Set YTD Won")
    fetch_atp$"Hard YTD Won" <- as.numeric(fetch_atp$"Hard YTD Won")
    fetch_atp$"Clay YTD Won" <- as.numeric(fetch_atp$"Clay YTD Won")
    fetch_atp$"Grass YTD Won" <- as.numeric(fetch_atp$"Grass YTD Won")
    fetch_atp$"Carpet YTD Won" <- as.numeric(fetch_atp$"Carpet YTD Won")
    fetch_atp$"Indoor YTD Won" <- as.numeric(fetch_atp$"Indoor YTD Won")
    fetch_atp$"Outdoor YTD Won" <- as.numeric(fetch_atp$"Outdoor YTD Won")
    fetch_atp$"After 1st Set Win YTD Won" <- as.numeric(fetch_atp$"After 1st Set Win YTD Won")
    fetch_atp$"After 1st Set Loss YTD Won" <- as.numeric(fetch_atp$"After 1st Set Loss YTD Won")
    fetch_atp$"vs. Righties YTD Won" <- as.numeric(fetch_atp$"vs. Righties YTD Won")
    fetch_atp$"vs. Lefties YTD Won" <- as.numeric(fetch_atp$"vs. Lefties YTD Won")
    fetch_atp$"Overall YTD Lost" <- as.numeric(fetch_atp$"Overall YTD Lost")
    fetch_atp$"Grand Slams YTD Lost" <- as.numeric(fetch_atp$"Grand Slams YTD Lost")
    fetch_atp$"Masters 1000 YTD Lost" <- as.numeric(fetch_atp$"Masters 1000 YTD Lost")
    fetch_atp$"Tiebreaks YTD Lost" <- as.numeric(fetch_atp$"Tiebreaks YTD Lost")
    fetch_atp$"Versus Top 10 YTD Lost" <- as.numeric(fetch_atp$"Versus Top 10 YTD Lost")
    fetch_atp$"Finals YTD Lost" <- as.numeric(fetch_atp$"Finals YTD Lost")
    fetch_atp$"Deciding Set YTD Lost" <- as.numeric(fetch_atp$"Deciding Set YTD Lost")
    fetch_atp$"5th Set YTD Lost" <- as.numeric(fetch_atp$"5th Set YTD Lost")
    fetch_atp$"Hard YTD Lost" <- as.numeric(fetch_atp$"Hard YTD Lost")
    fetch_atp$"Clay YTD Lost" <- as.numeric(fetch_atp$"Clay YTD Lost")
    fetch_atp$"Grass YTD Lost" <- as.numeric(fetch_atp$"Grass YTD Lost")
    fetch_atp$"Carpet YTD Lost" <- as.numeric(fetch_atp$"Carpet YTD Lost")
    fetch_atp$"Indoor YTD Lost" <- as.numeric(fetch_atp$"Indoor YTD Lost")
    fetch_atp$"Outdoor YTD Lost" <- as.numeric(fetch_atp$"Outdoor YTD Lost")
    fetch_atp$"After 1st Set Win YTD Lost" <- as.numeric(fetch_atp$"After 1st Set Win YTD Lost")
    fetch_atp$"After 1st Set Loss YTD Lost" <- as.numeric(fetch_atp$"After 1st Set Loss YTD Lost")
    fetch_atp$"vs. Righties YTD Lost" <- as.numeric(fetch_atp$"vs. Righties YTD Lost")
    fetch_atp$"vs. Lefties YTD Lost" <- as.numeric(fetch_atp$"vs. Lefties YTD Lost")
    fetch_atp$"Overall Career Won" <- as.numeric(fetch_atp$"Overall Career Won")
    fetch_atp$"Grand Slams Career Won" <- as.numeric(fetch_atp$"Grand Slams Career Won")
    fetch_atp$"Masters 1000 Career Won" <- as.numeric(fetch_atp$"Masters 1000 Career Won")
    fetch_atp$"Tiebreaks Career Won" <- as.numeric(fetch_atp$"Tiebreaks Career Won")
    fetch_atp$"Versus Top 10 Career Won" <- as.numeric(fetch_atp$"Versus Top 10 Career Won")
    fetch_atp$"Finals Career Won" <- as.numeric(fetch_atp$"Finals Career Won")
    fetch_atp$"Deciding Set Career Won" <- as.numeric(fetch_atp$"Deciding Set Career Won")
    fetch_atp$"5th Set Career Won" <- as.numeric(fetch_atp$"5th Set Career Won")
    fetch_atp$"Hard Career Won" <- as.numeric(fetch_atp$"Hard Career Won")
    fetch_atp$"Clay Career Won" <- as.numeric(fetch_atp$"Clay Career Won")
    fetch_atp$"Grass Career Won" <- as.numeric(fetch_atp$"Grass Career Won")
    fetch_atp$"Carpet Career Won" <- as.numeric(fetch_atp$"Carpet Career Won")
    fetch_atp$"Indoor Career Won" <- as.numeric(fetch_atp$"Indoor Career Won")
    fetch_atp$"Outdoor Career Won" <- as.numeric(fetch_atp$"Outdoor Career Won")
    fetch_atp$"After 1st Set Win Career Won" <- as.numeric(fetch_atp$"After 1st Set Win Career Won")
    fetch_atp$"After 1st Set Loss Career Won" <- as.numeric(fetch_atp$"After 1st Set Loss Career Won")
    fetch_atp$"vs. Righties Career Won" <- as.numeric(fetch_atp$"vs. Righties Career Won")
    fetch_atp$"vs. Lefties Career Won" <- as.numeric(fetch_atp$"vs. Lefties Career Won")
    fetch_atp$"Overall Career Lost" <- as.numeric(fetch_atp$"Overall Career Lost")
    fetch_atp$"Grand Slams Career Lost" <- as.numeric(fetch_atp$"Grand Slams Career Lost")
    fetch_atp$"Masters 1000 Career Lost" <- as.numeric(fetch_atp$"Masters 1000 Career Lost")
    fetch_atp$"Tiebreaks Career Lost" <- as.numeric(fetch_atp$"Tiebreaks Career Lost")
    fetch_atp$"Versus Top 10 Career Lost" <- as.numeric(fetch_atp$"Versus Top 10 Career Lost")
    fetch_atp$"Finals Career Lost" <- as.numeric(fetch_atp$"Finals Career Lost")
    fetch_atp$"Deciding Set Career Lost" <- as.numeric(fetch_atp$"Deciding Set Career Lost")
    fetch_atp$"5th Set Career Lost" <- as.numeric(fetch_atp$"5th Set Career Lost")
    fetch_atp$"Hard Career Lost" <- as.numeric(fetch_atp$"Hard Career Lost")
    fetch_atp$"Clay Career Lost" <- as.numeric(fetch_atp$"Clay Career Lost")
    fetch_atp$"Grass Career Lost" <- as.numeric(fetch_atp$"Grass Career Lost")
    fetch_atp$"Carpet Career Lost" <- as.numeric(fetch_atp$"Carpet Career Lost")
    fetch_atp$"Indoor Career Lost" <- as.numeric(fetch_atp$"Indoor Career Lost")
    fetch_atp$"Outdoor Career Lost" <- as.numeric(fetch_atp$"Outdoor Career Lost")
    fetch_atp$"After 1st Set Win Career Lost" <- as.numeric(fetch_atp$"After 1st Set Win Career Lost")
    fetch_atp$"After 1st Set Loss Career Lost" <- as.numeric(fetch_atp$"After 1st Set Loss Career Lost")
    fetch_atp$"vs. Righties Career Lost" <- as.numeric(fetch_atp$"vs. Righties Career Lost")
    fetch_atp$"vs. Lefties Career Lost" <- as.numeric(fetch_atp$"vs. Lefties Career Lost")
    fetch_atp$player <- players
    options(warn = warn)
    
    fetch_atp
} 
