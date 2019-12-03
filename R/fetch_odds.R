#' Fetch Odds Data for Slam Matches
#'
#' @export
fetch_odds <- function(atp = T){
	
	events <- c(
		"http://www.tennis-data.co.uk/ausopen.php",
		"http://www.tennis-data.co.uk/frenchopen.php",
		"http://www.tennis-data.co.uk/usopen.php",
		"http://www.tennis-data.co.uk/wimbledon.php"
	)
	
	
	pages <- lapply(events, read_html)
	
	csv_files <- lapply(pages, function(x){
		refs <- x %>% html_nodes("a") %>% html_attr("href")
	file.path("http://www.tennis-data.co.uk/", grep("[0-9][0-9][0-9][0-9].*csv", refs, val = T))
	})
	
	csv_files <- unlist(csv_files)
	
	if(atp)
		files <- csv_files[!grepl("[0-9][0-9][0-9][0-9]w", csv_files)]
	else
		files <- csv_files[grepl("[0-9][0-9][0-9][0-9]w", csv_files)]
	
	
	data <-  lapply(files, function(x) read.csv(x, stringsAsFactors = F, header = T))
	
	data_names <- unique(unlist(lapply(data, names)))
	
	data <- do.call("rbind", lapply(data, function(x){
		for(i in data_names)
			if(!(i %in% names(x)))
				x[,i] <- NA
		x
	}))
	
	data$Date <- dmy(data$Date)
	data$Location <- sub(" $", "", data$Location)
	data$Tournament <- sub(" $", "", data$Tournament)
	
		
	data$id <- paste(
		data$Location, 
		data$Date,
		data$Winner,
		data$Loser,
		sep = "-"
	)
	
data
}