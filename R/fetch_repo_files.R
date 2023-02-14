#' Scrapes CSV files from GITHUB repository
#' 
#' @param. url The repo URL address
#' 
#' @export
fetch_repo_files <- function(url){
	
	page <- read_html(url) 

	files <- page %>%
#		html_nodes("td.content") %>%
		html_nodes("a") %>%
		html_attr("href")
	
	# Returns CSV
grep("\\.csv", files, val = T)
}