#' Download Rankings from ATP Tour Website
#'
#' This function extracts ATP singles rankings for a particular date and ranking group
#'
#' @param date A character date for extracting rankings, YYYY-MM-DD
#' @param min_rank A numeric for the highest ranked player to include
#' @param max_rank A numeric for the lowest ranked player to include
#'
#' @export
fetch_atp_rankings <- function(date, min_rank = 1, max_rank = 100){

	url <- "http://www.atpworldtour.com/en/rankings/singles?rankDate=DATE&rankRange=RANK&countryCode=all"

	rank <- paste(min_rank, max_rank, sep = "-")
	
	url <- sub("DATE", date, url)
	url <- sub("RANK", rank, url)
	
	rankings <- readLines(url)
	
	name_index <- grep("en/players.*[a-z][0-9]+.*>[A-Z]", rankings)
	name <- sub("(.*en/players.*>)([A-Z].*)(</a>.*)", "\\2", rankings[name_index])
	
	age <- sub("([0-9]+)(\t.*)","\\1",rankings[name_index + 2])
	
	ranking_points <- sub("(.*rankings.*>)([0-9].*)(</a.*)","\\2",rankings[name_index + 4])
	tournaments_played <- sub("(.*player-activity.*>)([0-9].*)(</a.*)","\\2",rankings[name_index + 6])

data.frame(
	player = name,
	date = date,
	rank = min_rank : max_rank,
	age = age,
	points = ranking_points
)

}