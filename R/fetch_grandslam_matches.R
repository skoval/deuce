fetch_grandslam_matches <- function (player, year = NULL) 
{
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    if (is.null(year)) 
        year <- 0
    call <- collapse("http://www.atpworldtour.com/", call, "?t=pa&y=year&m=s&e=gs#")
    call <- sub("year", year, call)
    call <- url(call)
    lines <- readLines(call, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    close(call)
   
    match_lines <- grep("Match-Facts-Pop-Up.aspx?", lines)
    match_lines_split <- strsplit(lines[match_lines], split = "'")
    match_ids <- sapply(match_lines_split, function(x) x[grep("Match-Facts", x)])

paste("http://www.atpworldtour.com", match_ids, sep = "")
}