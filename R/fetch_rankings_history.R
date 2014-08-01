fetch_rankings_history <- function(player) {
    
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    call <- url(collapse("http://www.atpworldtour.com/", call, "?t=rh"))
    lines <- readLines(call, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    close(call)
    
    SinglesLine <- grep("Singles", lines)
    DoublesLine <- grep("Doubles", lines)
    StartLine <- SinglesLine[SinglesLine + 1 == DoublesLine]
    
    lines <- lines[StartLine:length(lines)]
    
    DatePattern <- "([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9])"
    DateSubPattern <- "(.*>)([0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9])(<.*)"
    Dates <- sub(DateSubPattern, "\\2", lines[grep(DatePattern, lines)])
    
    RankSubPattern <- "(.*>)(.*)(<.*)"  # WHAT IF BLANK?
    Ranks <- sub(RankSubPattern, "\\2", lines[grep(DatePattern, lines) + 1])
    Ranks <- as.numeric(sub("([0-9])(,*)?([0-9]*)(T)", "\\1\\3", Ranks))
    
    AsDate <- as.Date(Dates, "%d.%m.%Y")
    Dates <- strsplit(Dates, split = ".", fixed = TRUE)
    
    Day <- as.numeric(sapply(Dates, function(x) x[1]))
    Month <- as.numeric(sapply(Dates, function(x) x[2]))
    Year <- as.numeric(sapply(Dates, function(x) x[3]))
    
    Rankings <- data.frame(day = Day, month = Month, year = Year, rank = Ranks, date = AsDate)
    
    Rankings <- na.exclude(Rankings)
    
    Rankings
} 
