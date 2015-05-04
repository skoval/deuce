fetch_rankings <- function(dd.mm.yyyy, min.rank = 0, max.rank = 100) {
    
    fetch_rankings_apply <- function(dd.mm.yyyy, rank) {
        
        the_url <- "http://www.atpworldtour.com/Rankings/Singles.aspx?d=DD.MM.YYYY&r=HIGHEST&c=#"  # ALL COUNTRIES
        
        rankings_url <- sub("DD.MM.YYYY", dd.mm.yyyy, the_url, fixed = TRUE)
        rankings_url <- sub("HIGHEST", rank, rankings_url)
        
        lines <- readLines(con = rankings_url, warn = FALSE)
        
        name_pattern <- "(.*>)(.*)(</a>.*)"
        change_pattern <- "(.*>)(-?[0-9]+)(</td.*)"
        rank_pattern <- "(.*>)([0-9]+)(</span.*)"
        country_pattern <- "(.*\\()([A-Z][A-Z][A-Z])(\\))"
        
        names <- sub("&nbsp;", " ", sub(name_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines) + 2]))
        countries <- sub(country_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines) + 2])
        points <- sub(name_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines) + 6])
        weekchange <- sub(change_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines) + 7])
        tournaments <- sub(name_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines) + 8])
        rank <- sub(rank_pattern, "\\2", lines[grep("[[:punct:]]rank[[:punct:]]", lines)])
        
        data.frame(player = names, 
        		     rank = as.numeric(rank), 
        		     country = countries,
        		     points = as.numeric(sub(",","",points)), 
        		     weekchange = as.numeric(weekchange), 
        		     tournaments = as.numeric(tournaments), 
        		     stringsAsFactors = FALSE)
    }
    
    result <- do.call("rbind", lapply(seq(0, max.rank, by = 100) + 1, fetch_rankings_apply, dd.mm.yyyy = dd.mm.yyyy))
    
    result[min.rank:max.rank, ]
} 
