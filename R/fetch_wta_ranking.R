#' Download Current WTA Rankings
#'
#' This function extracts current WTA rankings
#'
#' @param min_rank A numeric for the highest ranked player to include
#' @param max_rank A numeric for the lowest ranked player to include
#'
#' @examples
#' fetch_wta_rankings(1, 150)
#'
#' @export
#'
#' @return data frame of rankings for the specified week
##' \itemize{
##'  \item player. Character name of player
##'  \item nation. Character name of player
##'  \item points. Numeric of ranking points
##'  \item tournaments. Numeric of tournaments played
##'  \item rank. Numeric rank
##'  \item date. Date object of Monday rankings were updated
##'}
##'
fetch_wta_rankings <- function(min_rank = 1, max_rank = 100){

	url <- "https://www.flashscore.com/tennis/rankings/wta/#"

	webpage <- read_html(url)

	table <- webpage %>% 
		html_nodes(css = "table") %>%
		html_table(header = T)
	
	date <- today()
		
	table <- table[[1]]

	variables <- table[1,]
	
	table <- table[-1,]
	
	names(table) <- variables
	
	table$Rank <- 1:nrow(table)
	table <- table[,-1]
	table <- subset(table, Rank >= min_rank & Rank <= max_rank)
	
	names(table) <- c("player", "nation", "points", "tournaments", "rank")

	table$date <- date
	table$points <- as.numeric(table$points)
	table$tournaments <- as.numeric(table$tournaments)	
	
table
}