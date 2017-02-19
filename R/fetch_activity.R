#' Download Player Activity
#'
#' This function extracts match activity and results for ATP players
#'
#' @param player Character name of ATP player
#' @param year Numeric year or "all" for all years
#'
#'@examples
#' fetch_activity("Rafael Nadal", 2016)
#' fetch_activity("Alexander Zverev", "all")
#'
#' @export
#'
#' @return data frame of match activity and results
##' \itemize{
##'  \item name. Character name of tournament
##'  \item location. Character of tournament city and country
##'  \item start_date. Date object of start of tournament
##'  \item end_date. Date object of end of tournament
##'  \item draw. Numeric of main draw size
##'  \item matches. Numeric of total matches played in main draw
##'  \item surface. Character of surface
##'  \item prize. Character of prize money awarded
##'  \item score. Character of match score
##'  \item round. Character of match round
##'  \item winner. Numeric indicator if player won match
##'  \item player. Character name of player
##'  \item player_rank. Numeric of player rank at start of event
##'  \item opponent. Character name of opponent
##'  \item opponent_rank. Numeric of opponent rank at start of event
##'  \item player1. Numeric of player games won in first set, NA if set not played
##'  \item player2. Numeric of player games won in second set, NA if set not played
##'  \item player3. Numeric of player games won in third set, NA if set not played
##'  \item player4. Numeric of player games won in fourth set, NA if set not played
##'  \item player5. Numeric of player games won in fifth set, NA if set not played
##'  \item opponent1. Numeric of opponent games won in first set, NA if set not played
##'  \item opponent2. Numeric of opponent games won in second set, NA if set not played
##'  \item opponent3. Numeric of opponent games won in third set, NA if set not played
##'  \item opponent4. Numeric of opponent games won in fourth set, NA if set not played
##'  \item opponent5. Numeric of opponent games won in fifth set, NA if set not played
##'  \item TBplayer1. Numeric of points won in first set tiebreak, NA if not played
##'  \item TBplayer2. Numeric of points won in second set tiebreak, NA if not played
##'  \item TBplayer3. Numeric of points won in third set tiebreak, NA if not played
##'  \item TBplayer4. Numeric of points won in fourth set tiebreak, NA if not played
##'  \item TBplayer5. Numeric of points won in fifth set tiebreak, NA if not played
##'  \item TBopponent1. Numeric of points won in first set tiebreak, NA if not played
##'  \item TBopponent2. Numeric of points won in second set tiebreak, NA if not played
##'  \item TBopponent3. Numeric of points won in third set tiebreak, NA if not played
##'  \item TBopponent4. Numeric of points won in fourth set tiebreak, NA if not played
##'  \item TBopponent5. Numeric of points won in fifth set tiebreak, NA if not played
##' }
##'
fetch_activity <- function(player, year){

	surface <- function(string) {
	    if (length(grep("Hard", string)) == 1) 
	        "Hard" else if (length(grep("Grass", string)) == 1) 
	        "Grass" else "Clay"
	}
	
	surface <- Vectorize(surface)
	
	losses <- function(Lines, Start, Stop) length(grep("L&", Lines[Start:Stop]))
	
	wins <- function(Lines, Start, Stop) {
	    length(grep("W&", Lines[Start:Stop])) - length(grep("Bye", Lines[Start:Stop]))
	}

	warn.source <- options("warn")$warn
	on.exit(options(warn = warn.source))
	options(warn = -1)
	
    data(atp_player_sites)
    
    
    site <- atp_player_sites$site[atp_player_sites$player == player]
    
    if(length(site) == 0){
    	site <- atp_player_sites$site[grep(player, atp_player_sites$player)][1]
    }
    
    if(length(site) == 0)
    	stop("Player not found. Check spelling.")
    	
    site <- paste("http://www.atpworldtour.com/", site, sep = "")
    site <- sub("overview", "player-activity?year=YEAR&matchType=singles", site)
    site <- sub("YEAR", year, site)

	
    lines <- readLines(site)
    
    #tourneys_start <- grep("categorystamps", lines)
	tourneys_start <- grep("images/tourtypes", lines)
    tourneys_stop <- grep("This Event", lines)

    tournament_list <- mapply(function(x, y){lines[x:y]}, x= tourneys_start, y = tourneys_stop )

    extract_fields <- function(lines){

		
        item_values <- grep("item-value", lines) 

	
        tournament_name <- grep("[A-Z]", lines[grep("tourney-title", lines):length(lines)], val = TRUE)[1] # First occurrence
        tournament_name <- sub("(.*title.*>)(.*)(</a.*)", "\\2", tournament_name)
        tournament_name <- gsub("\t", "", tournament_name)


        date <-  grep("([0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9])", lines)[1] # First dates
        location <-  gsub("\t", "", lines[grep("tourney-location", lines)[1] + 1])

        draw <- grep("SGL", lines)[1]
        draw <- item_values[item_values > draw][1] + 1

        surface1 <- grep("door", lines)[1] 
        surface2 <- grep("(Hard|Grass|Clay)", lines)[1]
        prize <- grep("Financial", lines)[1]
        prize <- item_values[item_values > prize][1] + 1
        prize <- gsub(",", "", lines[prize])

        if(grepl("[0-9]", prize))
            prize <- sub("&#163;", "L", sub("(.*[0-9])(\t.*)", "\\1", prize), fixed = TRUE)
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

        matches <- grep("match-stats|not-in-system.*[0-9]|W/O", lines)

        scores <- gsub("<sup>", "@", lines[matches])
        scores <- gsub("</sup>", "!", scores)
        scores <- sub("(.*>)([0-9].*)(</a>.*)", "\\2", scores)

        games <- strsplit(scores, split =  " ")
        score_str <- sapply(games, function(x) paste(sub("!", ")", sub("@", "(", x)), collapse = "/"))
        games_won <- lapply(games, function(x) substr(x, 1, 1))
        games_lost <- lapply(games, function(x) substr(x, 2, 2))

       tiebreak_won <- lapply(games, function(x){
            if(any(grepl("@", x))){
                points <- sapply(x, function(y){
                    if(grepl("@", y)){
                      point <- as.numeric(sub("(.*@)([0-9]+)(!.*)", "\\2", y))
                       if(as.numeric(substr(y, 1, 1)) == 7){
                        point <- ifelse(point + 2 > 7, point + 2, 7)
                      }
                    }
                    else{
                      point <- NA  
                    }
             point
            })
            }
            else{
                rep(NA, length(x))
            }
        })
         tiebreak_lost <- lapply(games, function(x){
            if(any(grepl("@", x))){
                points <- sapply(x, function(y){
                    if(grepl("@", y)){
                      point <- as.numeric(sub("(.*@)([0-9]+)(!.*)", "\\2", y))
                       if(as.numeric(substr(y, 1, 1)) != 7){
                        point <- ifelse(point + 2 > 7, point + 2, 7)
                      }
                    }
                    else{
                      point <- NA  
                    }
             point
            })
            }
            else{
                rep(NA, length(x))
            }
        })
        
        match_urls <- sub("(.*)(en/.*match-stats)(.*)", "\\2", lines[matches]) 

        # Checks for W/O
        if(any(grepl("W/O", lines))){
            walkover <- grep("W/O", lines[matches])
            score_str[walkover] <- 'W/O'
            games_won[walkover] <- NA
            games_lost[walkover] <- NA
            tiebreak_won[walkover] <- NA
            tiebreak_lost[walkover] <- NA
            match_urls[walkover] <- NA
        }
        
        event_line <- grep("activity-tournament-caption", lines, val = TRUE)

        if(grepl("This Event Points: [0-9]", event_line))
            points <- sub("(.*This Event Points: )([0-9]+)(,.*)", "\\2", event_line)
        else
            points <- NA

        if(grepl("ATP Ranking", event_line))
            player_ranking <- sub("(.*ATP Ranking: )([0-9]+)(,.*)", "\\2", event_line)
        else
            player_ranking <- NA


        Round_Pattern <- "Olympic|Round of|Round Robin|Final|Round Qualifying"
        Rounds <- grep(Round_Pattern, lines)
        Rounds <- Rounds[Rounds > grep("mega-table", lines)]

        Player_Lines <- grep("mega-player-name.*>[^(Bye)]", lines) - 1
        Rounds <- Rounds[1:length(Player_Lines)] 
        Round_Lines <- Rounds 

        Ranks <- mapply(function(x, y){
            where_ranks <- grepl("[0-9]", lines[x:y]) & !grepl("Round", lines[x:y])
            sub("([0-9]+)(.*)", "\\1", lines[x:y][where_ranks][1])
            }, x = Round_Lines, y = Player_Lines)
        Outcome <- mapply(function(x, y){
            win <- any(grepl("^W", lines[(x+1):y]))
            ifelse(win, 1, 0)
            }, x = Player_Lines, y = Player_Lines + 5)
        Rounds <- sub("(.*>)(.*)(<.*)", "\\2", lines[Rounds])

        if(length(matches) != length(Rounds))
            Rounds <- Rounds[1:length(matches)]

        opponents <- sub("(.*>)([A-Z].*)(</a>.*)", "\\2", grep("mega-player-name.*>[^(Bye)]", lines, val = TRUE))

        # activity-tournament-caption

    data.frame(
        name = tournament_name,
        location = location,
        start_date = as.Date(start_date, format = "%Y.%m.%d"),
        end_date = as.Date(end_date, format = "%Y.%m.%d"),  
        draw = as.numeric(draw_size),
        matches = as.numeric(draw_size) - 1,
        surface = surface_type,
        prize = prize,
        score = score_str,
        round = Rounds, 
        winner = Outcome,
        player = player,
        player_rank = as.numeric(player_ranking),
        opponent = opponents,
        opponent_rank = as.numeric(Ranks),
        player1 = sapply(games_won, function(x) as.numeric(x[1])),
        player2 = sapply(games_won, function(x) as.numeric(x[2])),
        player3 = sapply(games_won, function(x) as.numeric(x[3])),
        player4 = sapply(games_won, function(x) as.numeric(x[4])),
        player5 = sapply(games_won, function(x) as.numeric(x[5])),
        opponent1 = sapply(games_lost, function(x) as.numeric(x[1])),
        opponent2 = sapply(games_lost, function(x) as.numeric(x[2])),
        opponent3 = sapply(games_lost, function(x) as.numeric(x[3])),
        opponent4 = sapply(games_lost, function(x) as.numeric(x[4])),
        opponent5 = sapply(games_lost, function(x) as.numeric(x[5])),
        TBplayer1 = sapply(tiebreak_won, function(x) as.numeric(x[1])),
        TBplayer2 = sapply(tiebreak_won, function(x) as.numeric(x[2])),
        TBplayer3 = sapply(tiebreak_won, function(x) as.numeric(x[3])),
        TBplayer4 = sapply(tiebreak_won, function(x) as.numeric(x[4])),
        TBplayer5 = sapply(tiebreak_won, function(x) as.numeric(x[5])),
        TBopponent1 = sapply(tiebreak_lost, function(x) as.numeric(x[1])),
        TBopponent2 = sapply(tiebreak_lost, function(x) as.numeric(x[2])),
        TBopponent3 = sapply(tiebreak_lost, function(x) as.numeric(x[3])),
        TBopponent4 = sapply(tiebreak_lost, function(x) as.numeric(x[4])),
        TBopponent5 = sapply(tiebreak_lost, function(x) as.numeric(x[5])),
        row.names = NULL,
        stringsAsFactors = FALSE
    )
  }
  
  
do.call("rbind", lapply(tournament_list, extract_fields))
}