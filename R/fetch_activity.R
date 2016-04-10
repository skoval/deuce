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

fetch_activity <- function(player, year = NULL) {
    
    
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    
    if(is.null(year)) year <- 0
    
    call <- collapse("http://www.atpworldtour.com/", call, "?t=pa&y=year&m=s&e=0#")
    call <- sub("year", year, call)
    call <- url(call)
    
    lines <- readLines(call, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    close(call)
    
    if(year == 0)
	    year_pattern <- "\\.[1|2][0-9][0-9][0-9];"
	else
		year_pattern <- paste("\\.", year, ";", sep = "")
		
    tournaments <- grep(year_pattern, lines)
    points <- grep("This Event Points|ATP Ranking", lines)
    
    if ((length(tournaments) != length(points)) & year != 0) {
        year_pattern <- paste(".", as.numeric(year) - 1, ";", sep = "")
        tournaments <- sort(c(tournaments, grep(year_pattern, lines)))
    }
    
    date <- sub("(.*;)([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9])(;.*)", "\\2", lines[tournaments])
    tournament_names <- sub("(.*<strong>)(.*)(</strong>.*)", "\\2", lines[tournaments])
    result <- sub("(.*<td>)(.*)(</td>.*)", "\\2", lines[points - 14])
    
    start <- tournaments
    stop <- points
    
    W <- mapply(wins, Start = start, Stop = stop, MoreArgs = list(Lines = lines))
    L <- mapply(losses, Start = start, Stop = stop, MoreArgs = list(Lines = lines))
    
    the_date <- as.Date(date, "%d.%m.%Y")
    dates <- strsplit(date, split = ".", fixed = TRUE)
    
    day <- as.numeric(sapply(dates, function(x) x[1]))
    month <- as.numeric(sapply(dates, function(x) x[2]))
    year <- as.numeric(sapply(dates, function(x) x[3]))
    
    activity <- data.frame(day = day, month = month, year = year, date = the_date, tournament = tournament_names, result = result, wins = W, 
        losses = L, surface = as.character(surface(lines[tournaments])))
    
    activity
} 



fetch_activity(player, year){

    site <- atp_player_sites$site[atp_player_sites$player == player]
    site <- paste("http://www.atpworldtour.com/", site, sep = "")
    site <- sub("overview", "player-activity?year=YEAR", site)
    site <- sub("YEAR", year, site)

    lines <- readLines(site)


    tourneys_start <- grep("categorystamps", lines)
    tourneys_stop <- grep("This Event", lines)

    tournament_list <- mapply(function(x, y){lines[x:y]}, x= tourneys_start, y = tourneys_stop )

    extract_fields <- function(lines){

        item_values <- grep("item-value", lines) 

        tournament_name <- grep("[A-Z]", lines[grep("tourney-title", lines):length(lines)], val = TRUE)[1] # First occurrence
        tournament_name <- sub("(.*title.*>)(.*)(</a.*)", "\\2", lines[tournament_name])
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

        matches <- grep("match-stats", lines)

        if(length(matches) == 0)
            matches <- grep("not-in-system.*[0-9]", lines)
        
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
        
        event_line <- grep("activity-tournament-caption", lines, val = TRUE)

        if(grepl("This Event Points: [0-9]", event_line))
            points <- sub("(.*This Event Points: )([0-9]+)(,.*)", "\\2", event_line)
        else
            points <- NA

        if(grepl("ATP Ranking", event_line))
            player_ranking <- sub("(.*ATP Ranking: )([0-9]+)(,.*)", "\\2", event_line)
        else
            player_ranking <- NA


        # Order: round -> rank -> name -> outcome -> score

        Round_Pattern <- "Round of|Round Robin|Final"
        Rounds <- grep(Round_Pattern, lines)
        Rounds <- Rounds[Rounds > grep("mega-table", lines)]

        Round_Lines <- Rounds
        Player_Lines <- grep("mega-player-name", lines) - 1

        Ranks <- mapply(function(x, y){
            where_ranks <- grepl("[0-9]", lines[x:y]) & !grepl("Round", lines[x:y])
            sub("([0-9]+)(.*)", "\\1", lines[x:y][where_ranks][1])
            }, x = Round_Lines, y = Player_Lines)

        
        Outcome <- mapply(function(x, y){
            win <- any(grepl("W", lines[(x+1):y]))
            ifelse(win, 1, 0)
            }, x = Player_Lines, y = Player_Lines + 5)


        Rounds <- sub("(.*>)(.*)(<.*)", "\\2", lines[Rounds])

        if(length(matches) != length(Rounds))
            Rounds <- Rounds[1:length(matches)]

        opponents <- sub("(.*>)([A-Z].*)(</a>.*)", "\\2", grep("mega-player-name", lines, val = TRUE))

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
        player_rank = player_ranking,
        opponent = opponents,
        opponent_rank = Ranks,
        player1 = sapply(games_won, function(x) x[1]),
        player2 = sapply(games_won, function(x) x[2]),
        player3 = sapply(games_won, function(x) x[3]),
        player4 = sapply(games_won, function(x) x[4]),
        player5 = sapply(games_won, function(x) x[5]),
        opponent1 = sapply(games_lost, function(x) x[1]),
        opponent2 = sapply(games_lost, function(x) x[2]),
        opponent3 = sapply(games_lost, function(x) x[3]),
        opponent4 = sapply(games_lost, function(x) x[4]),
        opponent5 = sapply(games_lost, function(x) x[5]),
        TBplayer1 = sapply(tiebreak_won, function(x) x[1]),
        TBplayer2 = sapply(tiebreak_won, function(x) x[2]),
        TBplayer3 = sapply(tiebreak_won, function(x) x[3]),
        TBplayer4 = sapply(tiebreak_won, function(x) x[4]),
        TBplayer5 = sapply(tiebreak_won, function(x) x[5]),
        TBopponent1 = sapply(tiebreak_lost, function(x) x[1]),
        TBopponent2 = sapply(tiebreak_lost, function(x) x[2]),
        TBopponent3 = sapply(tiebreak_lost, function(x) x[3]),
        TBopponent4 = sapply(tiebreak_lost, function(x) x[4]),
        TBopponent5 = sapply(tiebreak_lost, function(x) x[5])
    )
 }

    round
    opponent_rank
    player_rank
    opponent
    points
    result
    score




}