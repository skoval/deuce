# url <- "http://www.atpworldtour.com/en/search-results/players?page=NUM"

# atp_player_sites <- do.call("rbind", lapply(1:50, function(pagenum){
	
	# site <- readLines(sub("NUM", pagenum, url))
	# top_player_results <- grep("result-name", site)
	# top_player_urls <- sub("(.*)(en/players.*overview)(.*)", "\\2", site[top_player_results + 1])
	# player_names <- gsub("\t", "", site[top_player_results + 2])
	
	# data.frame(
		# site = top_player_urls,
		# player = player_names,
	# stringsAsFactors = FALSE
	# )
 # })
# )

# atp_player_sites$player <- gsub("-", " ", atp_player_sites$player)
# atp_player_sites$player <- sub("&#39;", "", atp_player_sites$player, fixed = TRUE)

# save(atp_player_sites, file = "~/Software/deuce/data/atp_player_sites.RData")