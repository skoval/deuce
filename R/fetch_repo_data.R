#' @export
fetch_repo_data <- function(file, ...){
	file <- file.path("https://raw.githubusercontent.com", str_remove(file, "^/"))
	file <- str_remove(file, ".blob")
read.csv(file, ...)
}