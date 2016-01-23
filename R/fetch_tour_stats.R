#' Download Performance Statistics for ATP
#'
#' This function extracts performance statistics for the ATP 
#'
#' @param stat A character variable that is one of the following: "Aces", "1st Serve", "1st Serve Won", "2nd Serve #' Won", "Service Games Won",  "Break Points Saved",  "1st Serve Return Won", "2nd Serve Return Won", or "Break Points Converted")
#' @param year A character for the year or "all" for career stats
#' @param surface A character for the surface type (Clay, Hard, or Grass); use "all" for all surfaces
#'
#' @export
fetch_tour_stats <- function(stat = "Aces", year = "all", surface = "Clay"){
	
	base_url <- "http://www.atpworldtour.com/en/stats/STAT/YEAR/SURFACE/all/"
	
	stat <- ifelse(stat == "Aces", "aces",
		ifelse(stat == "1st Serve", "1st-serve",
		 ifelse(state == "1st Serve Won", "1st-serve-points-won",
		   ifelse(stat == "2nd Serve Won", "2nd-serve-points-won",
		     ifelse(stat == "Service Games Won", "service-games-won",
		      ifelse(stat == "Break Points Saved", "break-points-saved",
		        ifelse(stat == "1st Serve Return Won", "1st-serve-return-points-won",
		          ifelse(stat == "2nd Serve Return Won", "2nd-serve-return-points-won" ,
		            ifelse(stat == "Break Points Converted", "break-points-converted", "return-games-won")))))))))
	
	surface <- ifelse(surface == "Clay", "clay",
		ifelse(surface == "Grass", "grass", 
		 ifelse(surface == "Hard", "hard", "all")))
	
	base_url <- sub("STAT", stat, base_url)
	base_url <- sub("SURFACE", surface, base_url)
	base_url <- sub("YEAR", year, base_url)
	
	stat_lines <- readLines(base_url)
	
	name_index <- grep("en/players.*[a-z][0-9]+.*>[A-Z]", stat_lines)
	name <- sub("(.*en/players.*>)([A-Z].*)(</a>.*)", "\\2", stat_lines[name_index])
	
	number <- sub("(.*>)([0-9].*)(</td>)","\\2", stat_lines[name_index + 6])
	matches <- sub("(.*>)([0-9].*)(</td>)","\\2", stat_lines[name_index + 7])

data.frame(
	player = name,
	number = number,
	matches = matches
)	

}