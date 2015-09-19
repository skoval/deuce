#' Parses Set Score
#'
#' Adds semicolons to unprocessed strings of game outcomes in the \code{point_by_point} data set
#'
#' @param xstr a string of point outcomes
#'
#' @export
add_semicolon <- function(xstr){

	points <- strsplit(xstr, split = "")[[1]]
   	ace <- grepl("A", points)
	df <- grepl("D", points)
	points <- sub("D", "R", sub("A", "S",  points))
	x <- points == "S"
	xpoints <- points
	
	semicolon <- NULL

	for(i in 4:length(x)){
		# All possible end game scenarios
		if((sum(x[1:i], na.rm = TRUE) >= 4 & sum((1-x)[1:i], na.rm = TRUE) < 3) |
		    (sum((1-x)[1:i], na.rm = TRUE) >= 4 & sum(x[1:i], na.rm = TRUE) < 3) |
		    (sum(x[1:i], na.rm = TRUE) >= 4 & sum((1-x)[1:i], na.rm = TRUE) >= 3 &
		    (sum(x[1:i], na.rm = TRUE) - sum((1-x)[1:i], na.rm = TRUE)) >= 2 |
		     (sum((1-x)[1:i], na.rm = TRUE) >= 4 & sum(x[1:i], na.rm = TRUE) >= 3 &
		       (sum(x[1:i], na.rm = TRUE) - sum((1-x)[1:i], na.rm = TRUE)) <= -2))) {
			semicolon <- c(semicolon, i)
			x[1:i] <- NA
		}	
	}

	start <- c(1, semicolon[-length(semicolon)] + 1)
	end <- semicolon
	
	processed <- mapply(function(source, x, y) substr(source, x, y), 
		x = start, y = end, MoreArgs = list(source = xstr))

paste(unlist(processed), sep = "", collapse = ";")
}			