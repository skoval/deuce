atp_tournaments <- function(challenger = FALSE){
	
	if(challenger){
		tournaments <- readLines("http://www.atpworldtour.com/en/atp-challenger-tour/calendar")
		tourneys <- grep("tourney-title", tournaments)
		dates <- tourneys + 8
		draw <-  tourneys + 22
		surface <- tourneys + 40
		tier <- tourneys - 4
		tourneys <- tourneys + 1
		tier <- "Challenger"
	}
	else{
		tournaments <- readLines("http://www.atpworldtour.com/en/tournaments")
		tourneys <- grep("tourney-title", tournaments)
		dates <- tourneys + 5
		draw <-  tourneys + 19
		surface <- tourneys + 37
		tier <- tourneys - 4	
		tier_name <- sub("(.*categorystamps_)(.*)(_[0-9].*)", "\\2", tournaments[tier])		
		tier <- ifelse(tier_name == "1000s", "1000",
				ifelse(tier_name == "finals-pos", "Tour Finals",
					ifelse(tier_name == "grandslam", "Grand Slam", tier_name)))
	}
	
	tournament_names <- sub("(.*title.*>)(.*)(</a.*)", "\\2", tournaments[tourneys])
	
	start_date <- sub("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])(.*)","\\1",tournaments[dates])
	end_date <- sub("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9].*)(.*)([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])(.*)","\\3",tournaments[dates])
	draw_size <-  gsub("\t","",tournaments[draw])
	surface_types <- gsub("\t","",tournaments[surface])

data.frame(
	name = tournament_names,
	start_date = as.Date(start_date, format = "%Y.%m.%d"),
	end_date = as.Date(end_date, format = "%Y.%m.%d"),	
	draw = as.numeric(draw_size),
	matches = as.numeric(draw_size) - 1,
	surface = surface_types,
	tier = tier
)

}