#' Download Rankings from ATP Tour Website
#'
#' This function extracts ATP singles rankings for a particular date and ranking group
#'
#' @param date A character date for extracting rankings, YYYY-MM-DD
#' @param min_rank A numeric for the highest ranked player to include
#' @param max_rank A numeric for the lowest ranked player to include
#'
#' @examples
#' fetch_atp_rankings("2017-01-01")
#'
#' @export
#'
#' @return data frame of rankings for the specified week
##' \itemize{
##'  \item player. Character name of player
##'  \item date. Date object of Monday rankings were updated
##'  \item rank. Numeric rank
##'  \item age. Numeric of player age
##'  \item points. Numeric of ranking points
##'}
##'
fetch_atp_rankings <- function(date, min_rank = 1, max_rank = 100, singles = T){

	nearest_monday <- function(date){

		date <- ymd(date)	
		weekday <- weekdays(date)
		
		if(weekday == "Sunday")		
			date <- date + days(1)
		else if(weekday == "Tuesday")
			date <- date - days(1)
		else if(weekday == "Wednesday")
			date <- date - days(2)
		else if(weekday == "Thursday")
			date <- date - days(3)
		else if(weekday == "Friday")
			date <- date - days(4)
		else if(weekday == "Saturday")
			date <- date - days(5)	
		else	
			date <- date
			
	as.character(date)											
	}
	
	if(singles)
		url <- "http://www.atpworldtour.com/en/rankings/singles?rankDate=DATE&rankRange=RANK&countryCode=all"	
	else
	url <- "http://www.atpworldtour.com/en/rankings/doubles?rankDate=DATE&rankRange=RANK&countryCode=all"

	rank <- paste(min_rank, max_rank, sep = "-")
	
	date <- nearest_monday(date)
	
	url <- sub("DATE", date, url)
	url <- sub("RANK", rank, url)
	
	rankings <- read_html(url) %>% html_nodes("table") %>%  html_table()
	rankings <- rankings[[1]]
	
	rankings$Date <- date

rankings
}