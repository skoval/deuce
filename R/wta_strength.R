#' WTA Elo Ratings
#'
#' This dataset contains Elo ratings for the WTA tour from the Open Era to the present including 10K events and higher. 
#'
#' \itemize{
#'   \item player_name. A character name of the player
#'   \item opponent_name. A character name of the opponent
#'   \item player_rank. A numeric value of the player's rank at the start of the match
#'   \item opponent_rank. A numeric value of the opponent's rank at the start of the match
#'   \item player_age. A numeric value of the player's age at the start of the match
#'   \item opponent_age. A numeric value of the opponent's age at the start of the match
#'   \item tourney_start_date. A date object for the date of the start of the tournament
#'   \item tourney_name. A character name for the tournament
#'   \item tourney_level. A character description of the tournament level (e.g. Grand Slams, Masters, etc.)
#'   \item round. A character description of the the round the match was played in 
#'   \item match_num. A numeric value of the unique id of the match within the tournament
#'   \item win. A numeric value indicating if the player won the match 
#'   \item opponent_after_elo. A numeric value of the opponent Elo rating at the end of the match
#'   \item player_after_elo. A numeric value of the player Elo rating at the end of the match
#'   \item opponent_before_elo. A numeric value of the opponent Elo rating at the start of the match
#'   \item player_before_elo. A numeric value of the player Elo rating at the start of the match
#'   \item player_match_num. A numeric value of the player's career matches
#'   \item opponent_match_num. A numeric value of the opponent's career matches
#'   \item prediction. Numeric match win probability for the player
#'}
#'
#' @format A data frame with 776,134 rows and 19 variables
#' @name wta_elo
NULL