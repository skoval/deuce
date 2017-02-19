#' Download Current ATP Calendar
#'
#' This function downloads the ATP calendar
#'
#' @param challenger Logical indicator if fetching challenger or World Tour calendar
#'
#' @export
fetch_atp_tournaments <- function(challenger = FALSE){
	
	if(challenger){
		tournaments <- readLines("http://www.atpworldtour.com/en/atp-challenger-tour/calendar")
		tourneys <- grep("tourney-title", tournaments)
	}
	else{
		tournaments <- readLines("http://www.atpworldtour.com/en/tournaments")
		tourneys <- grep("tourney-title", tournaments)
	}
		tournament_list <- mapply(function(x, y){tournaments[x:y]}, x= tourneys, y =c(tourneys[-1], max(tourneys) + 100) )

	extract_fields <- function(lines){

		item_values <- grep("item-value", lines) 
		tournament_name <- grep("[A-Z]", lines)[1] # First occurrence
		tournament_name <- sub("(.*title.*>)(.*)(</a.*)", "\\2", lines[tournament_name])
		tournament_name <- gsub("\t", "", tournament_name)

		date <-  grep("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])", lines)[1] # First dates
		
		location <-  gsub("\t", "", lines[grep("tourney-location", lines)[1] + 1])

		tier <- grep("categorystamps", lines)[1]
		tier_name <- sub("(.*categorystamps_)(.*)(_[0-9].*)", "\\2", lines[tier])		
		tier <- ifelse(tier_name == "1000s", "1000",
				ifelse(tier_name == "finals-pos", "Tour Finals",
					ifelse(tier_name == "grandslam", "Grand Slam", tier_name)))

		draw <- grep("SGL", lines)[1]
		draw <- item_values[item_values > draw][1] + 1

		surface1 <- grep("door", lines)[1] 
		surface2 <- grep("(Hard|Grass|Clay)", lines)[1]
		
		prize <- grep("Financial", lines)[1]
		prize <- item_values[item_values > prize][1] + 1
		prize <- gsub(",", "", lines[prize])

		if(grepl("[0-9]", prize))
			prize <- sub("(.*[0-9])(\t.*)", "\\1", prize)
		else
			prize <- NA

		start_date <- sub("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])(.*)","\\1", lines[date])
		end_date <- sub("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9].*)(.*)([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])(.*)","\\3", lines[date])
		draw_size <-  gsub("\t","", lines[draw])

		if(!is.na(surface1)){
			surface_type <- ifelse(grepl("Outdoor", lines[surface1]), "Outdoor", "Indoor")
			if(!is.na(surface2)){
				surface_type2 <- ifelse(grepl("Hard", lines[surface2]), "Hard",
						ifelse(grepl("Clay", lines[surface2]), "Clay", "Grass"))
				surface_type <- paste(surface_type, surface_type2)
			}
		}
		else{
			if(!is.na(surface2)){
				surface_type <- ifelse(grepl("Hard", lines[surface2]), "Hard",
						ifelse(grepl("Clay", lines[surface2]), "Clay", "Grass"))
			}
			surface_type <- NA
		}

		tier <- ifelse(challenger, "challenger", tier)

	data.frame(
		name = tournament_name,
		location = location,
		start_date = as.Date(start_date, format = "%Y.%m.%d"),
		end_date = as.Date(end_date, format = "%Y.%m.%d"),	
		draw = as.numeric(draw_size),
		matches = as.numeric(draw_size) - 1,
		surface = surface_type,
		prize = prize
	)
	}

	tier <- grep("categorystamps", tournaments)
	ntournaments <- length(tournament_list)
	tier <- tier[(length(tier)-ntournaments+1):length(tier)]
	tier_name <- sub("(.*categorystamps_)(.*)(_[0-9].*)", "\\2", tournaments[tier])		
	tier <- ifelse(tier_name == "1000s", "1000",
				ifelse(tier_name == "finals-pos", "Tour Finals",
					ifelse(tier_name == "grandslam", "Grand Slam", tier_name)))


	
	obj <- do.call("rbind", lapply(tournament_list, extract_fields))

	if(challenger)
		obj$tier <- "challenger"
	else
		obj$tier <- tier
obj	
}
