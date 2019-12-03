#' @export
tidy_slam_point_by_point <- function(points, matches){
	
	fields <- unique(unlist(lapply(points, colnames)))
	
	points <- do.call("rbind", lapply(points, function(obj){
		if(length(setdiff(fields, colnames(obj))) > 0){
			names <- setdiff(fields, colnames(obj))
			for(var in names)
				obj[,var] <- NA
		}
	obj[,fields]
	}))

	fields <- unique(unlist(lapply(matches, colnames)))
	
	matches <- do.call("rbind", lapply(matches, function(obj){
		if(length(setdiff(fields, colnames(obj))) > 0){
			names <- setdiff(fields, colnames(obj))
			for(var in names)
				obj[,var] <- NA
		}
	obj[,fields]
	}))
	
	
	points %>% 
		inner_join(matches, by = "match_id")
}