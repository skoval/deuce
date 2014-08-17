fetch_wta_rankings <- function(dd.mm.yyyy, min.rank = 0, max.rank = 100) {
    
    fetch_rankings_apply <- function(dd.mm.yyyy, rank) {
        
        the_url <- "http://www.tennisexplorer.com/ranking/wta-women/2014/?type=WTA&date=DD.MM.YYYY&page=HIGHEST"  # ALL COUNTRIES
        
        dd.mm.yyyy <- sub("([0-9][0-9])(.)([0-9][0-9])(.)([0-9][0-9][0-9][0-9])","\\1-\\3-\\5", dd.mm.yyyy)
        rankings_url <- sub("DD.MM.YYYY", dd.mm.yyyy, the_url, fixed = TRUE)
        rankings_url <- sub("HIGHEST", rank, rankings_url)
        
        call <- url(rankings_url)
        lines <- readLines(con = call, warn = FALSE)
        close(call)
        
        index_pattern <- "\"rank first"
        name_pattern <- "(.*>)(.*)(</a>.*)"
        rank_pattern <- "(.*>)(.*)(\\.</td>)"
        country_pattern <- "(.*span>)(.*)(</a>.*)"
        point_pattern <- "(.*>)(.*)(</td>)"
       
       	index <- grep(index_pattern, lines)
       	rank <- sub(rank_pattern,"\\2", lines[index])
        names <- sub(name_pattern, "\\2", lines[index+1])
        country <- sub(country_pattern, "\\2", lines[index+2])
        points <- sub(point_pattern, "\\2", lines[index+3])
         
        data.frame(player = names, 
        		     rank = as.numeric(rank), 
        		     country = country, 
        		     points = as.numeric(points),
        		     stringsAsFactors = FALSE)
    }
    
    pages <- floor(max.rank / 50)
    
    if(max.rank %% 50 > 0) pages <- pages + 1
    
    result <- do.call("rbind", lapply(1:pages, fetch_rankings_apply, dd.mm.yyyy = dd.mm.yyyy))
    
result[min.rank:max.rank, ]
} 
