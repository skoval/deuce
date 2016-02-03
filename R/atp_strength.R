#' ATP Strength
#'
#' This dataset contains Elo ratings and break point conversion info for the ATP tour during the Open Era.
#'
#' \itemize{
#'   \item player. A character name of the player
#'   \item opponent. A character name of the opponent
#'   \item player_rank. A numeric value of the player's rank at the start of the match
#'   \item opponent_rank. A numeric value of the opponent's rank at the start of the match
#'   \item player_age. A numeric value of the player's age at the start of the match
#'   \item opponent_age. A numeric value of the opponent's age at the start of the match
#'   \item tourney_start_date. A date object for the date of the start of the tournament
#'   \item tourney_name. A character name for the tournament
#'   \item tourney_level. A character description of the tournament level (e.g. Grand Slams, Masters, etc.)
#'   \item round. A character description of the the round the match was played in 
#'   \item match_num. A numeric value of the unique id of the match within the tournament
#'   \item player_bp. A numeric value of the break points won within the match
#'   \item opponent_bp. A numeric value of the opponent's break points won within the match
#'   \item win. A numeric value indicating if the player won the match 
#'   \item opponent_bp_strength. A numeric value of the Pythagorean win expectation for the opponent
#'   \item player_bp_strength. A numeric value of the Pythagorean win expectation for the player
#'   \item opponent_after_strength. A numeric value of the opponent Elo rating at the end of the match
#'   \item player_after_strength. A numeric value of the player Elo rating at the end of the match
#'   \item opponent_before_strength. A numeric value of the opponent Elo rating at the start of the match
#'   \item player_before_strength. A numeric value of the player Elo rating at the start of the match
#'   \item player_12mo_obp. A numeric value of the break point wons by the player's opponents in the past 12 months 
#'   \item opponent_12mo_obp. A numeric value of the break point wons by the opponent's opponents in the past 12 months 
#'   \item player_12mo_bp. A numeric value of the break point wons by the player in the past 12 months 
#'   \item opponent_12mo_bp. A numeric value of the break point wons by the opponent in the past 12 months 
#'   \item player_match_num. A numeric value of the player's career matches
#'   \item opponent_match_num. A numeric value of the opponent's career matches
#'   \item match_id. A numeric id for the match (same as \code{atp_matches})
#'}
#'
#' @format A data frame with 288,126 rows and 27 variables
#' @name atp_strength
NULL