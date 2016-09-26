#' Download Rankings from ATP Tour Website
#'
#' This function extracts ATP singles rankings for a particular date and ranking group
#'
#' @param date A character date for extracting rankings
#' @param format The format of the supplied data e.g. 2016-05-22 = "%Y-%m-%d"
#' @param round Rankings are released on Mondays. If the date supplied is not a Monday,
#' should the function query the previous (round="prev") or next (round="next") Monday?
#' @param min_rank A numeric for the highest ranked player to include
#' @param max_rank A numeric for the lowest ranked player to include
#'
#' @export

fetch_atp_rankings <- function(date, format ="%Y-%m-%d", round="prev", min_rank = 1, max_rank = 100){
  
  if(!round %in% c("prev","next"))
    stop('Incorrect round argument: Must be either "prev" or "next"')
  
  check_date <- as.Date(date, format = format)
  
  if(is.na(check_date))
    stop('Incorrect date and/or date format!')
  
  if(weekdays(check_date)!="Monday"){
    check_date <- check_date + (-6 - as.POSIXlt(check_date)$wday %% -7 )
  }
  
  if(round=="next")
    check_date <- check_date + 7
  
  name_index <- NULL
  attempt <- 1
  
  url <- "http://www.atpworldtour.com/en/rankings/singles?rankDate=DATE&rankRange=RANK&countryCode=all"
  
  rank <- paste(min_rank, max_rank, sep = "-")
  
  url <- sub("RANK", rank, url)
  
  while(length(name_index)==0){
    
    if(attempt==3)
      stop("No rankings available. Change your date.")
    
    if(attempt==2)
    check_date <- check_date + ifelse(round=="prev",-7,7)
  
  rankings <- readLines(sub("DATE", check_date, url))
  
  name_index <- grep("en/players.*[a-z][0-9]+.*>[A-Z]", rankings)
  
  attempt <- attempt + 1
  }

  name <- sub("(.*en/players.*>)([A-Z].*)(</a>.*)", "\\2", rankings[name_index])
  
  age <- sub("([0-9]+)(\t.*)","\\1",rankings[name_index + 2])
  
  country <- gsub("\".*$","",gsub( ".*alt=\"", "", rankings[name_index - 5] ))
  
  ranking_points <- sub("(.*rankings.*>)([0-9].*)(</a.*)","\\2",rankings[name_index + 4])
  tournaments_played <- sub("(.*player-activity.*>)([0-9].*)(</a.*)","\\2",rankings[name_index + 6])
  
  data.frame(
    player = name,
    country = country,
    date = check_date,
    rank = min_rank : max_rank,
    age = age,
    points = ranking_points
  )
  
}
