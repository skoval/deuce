library(deuce)
library(httr)
library(jsonlite)


### SUPPLY ROOT FOR LOCAL DEUCE PACKAGE
package_root <- "~/Software/deuce"

### COLLECT AND SAVE PBP DATA
repo <- "https://github.com/JeffSackmann/tennis_pointbypoint"

files <- fetch_repo_files("JeffSackmann/tennis_pointbypoint")

point_by_point <- do.call("rbind", lapply(files, function(x) fetch_repo_data(x, stringsAsFactors = F, header = T)))

point_by_point <- tidy_point_by_point(point_by_point)

save(point_by_point, file = file.path(package_root, "data/point_by_point.RData"))


### COLLECT MATCH, RANKINGS, AND PLAYER DATA
repo <- "https://github.com/JeffSackmann/tennis_atp"

files <- fetch_repo_files("JeffSackmann/tennis_atp")

atp_players <- fetch_repo_data(grep("players", files, val = T), stringsAsFactors = F, header = F)

atp_players <- tidy_player_data(atp_players)

save(atp_players, file = file.path(package_root, "data/atp_players.RData"))

atp_rankings <- do.call("rbind", lapply(grep("rankings_[078]", files, val = T), function(x) fetch_repo_data(x, strings = FALSE, header = T, quote = "")))
# Deal with different format of ranking files
atp_rankings2 <- do.call("rbind", lapply(grep("rankings_[19c]", files, val = T), function(x) fetch_repo_data(x, strings = FALSE, header = F, quote = "")))

names(atp_rankings2) <- names(atp_rankings)

atp_rankings <- rbind(atp_rankings, atp_rankings2)

atp_rankings <- tidy_rankings_data(atp_rankings)

save(atp_rankings, file = file.path(package_root, "data/atp_rankings.RData"))

match_files <- grep("matches", files , val = T)
match_files <- match_files[!grepl("doubles", match_files)]

atp_matches <- do.call("rbind", lapply(match_files, function(x) fetch_repo_data(x, strings = FALSE, header = T)))

atp_matches <- tidy_match_data(atp_matches)

save(atp_matches, file = file.path(package_root, "data/atp_matches.RData"))

### COLLECT MATCH, RANKINGS, AND PLAYER DATA
repo <- "https://github.com/JeffSackmann/tennis_wta"

files <- fetch_repo_files("JeffSackmann/tennis_wta")

wta_players <- fetch_repo_data(grep("players", files, val = T), stringsAsFactors = F, header = F)

wta_players <- tidy_player_data(wta_players)

save(wta_players, file = file.path(package_root, "data/wta_players.RData"))

wta_rankings <- do.call("rbind", lapply(grep("rankings_[0-9c]", files, val = T), function(x) fetch_repo_data(x, strings = FALSE, header = FALSE, quote = "")))

wta_rankings <- tidy_rankings_data(wta_rankings)

save(wta_rankings, file = file.path(package_root, "data/wta_rankings.RData"))

wta_matches <- lapply(grep("matches", files, val = T), function(x) fetch_repo_data(x, strings = FALSE, header = T))

# Need to assign dataset names owing to some inconsistencies
fields <- unique(unlist(lapply(wta_matches, colnames)))

wta_matches <- do.call("rbind", lapply(wta_matches, function(x){
	if(length(setdiff(fields, colnames(x))) > 0){
		vars <- setdiff(fields, colnames(x))
		for(var in vars)
			x[,var] <- NA
	}
	x[,fields]
}))

wta_matches <- tidy_match_data(wta_matches, atp = F)

save(wta_matches, file = file.path(package_root, "data/wta_matches.RData"))


### COLLECT MCP
repo <- "https://github.com/JeffSackmann/tennis_MatchChartingProject"

files <- fetch_repo_files("JeffSackmann/tennis_MatchChartingProject")

match_files <- grep("wta_matches_[0-9]{4}", files, val = T)

matches <- do.call("rbind", lapply(match_files, function(x) fetch_repo_data(x, strings = FALSE, header = T, quote = "")))

matches$Date[matches$match_id == "1990409-W-Amelia_Island-F-Steffi_Graf-Arantxa_Sanchez_Vicario"] <- "19900409"

matches$match_date <- ymd(matches$Date)

points <- lapply(grep("points", files, val = T), function(x) fetch_repo_data(x, strings = FALSE, header = grepl("m-points", x), quote = ""))

names(points) <- grep("points", files, val = T)
colnames(points[[grep("w-points", names(points))]]) <- colnames(points[[grep("m-points", names(points))]])

points <- do.call("rbind", points)

mcp_points <- points %>%
	left_join(matches, by = "match_id")

save(mcp_points, file = file.path(package_root, "data/mcp_points.RData"))


### COLLECT ODDS DATA
atp_odds <- fetch_odds(atp = T)
wta_odds <- fetch_odds(atp = F)

save(atp_odds, file = file.path(package_root, "data/atp_odds.RData"))
save(wta_odds, file = file.path(package_root, "data/wta_odds.RData"))


### COLLECT SLAM PBP DATA
repo <- "https://github.com/JeffSackmann/tennis_slam_pointbypoint"

files <- fetch_repo_files("JeffSackmann/tennis_slam_pointbypoint")

gs_point_by_point <- lapply(grep("points", files, val = T), function(x) fetch_repo_data(x, stringsAsFactors = F, header = T))

gs_point_by_point_matches <- lapply(grep("matches", files, val = T), function(x) fetch_repo_data(x, stringsAsFactors = F, header = T))

gs_point_by_point <- tidy_slam_point_by_point(gs_point_by_point, gs_point_by_point_matches)

save(gs_point_by_point, file = file.path(package_root, "data/gs_point_by_point.RData"))
