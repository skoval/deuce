#' ATP Elo Ratings
#'
#' This dataset contains Elo ratings for the ATP tour from the Open Era to the present.
#'
#' \itemize{
#'   \item player_name. A character name of the player
#'   \item tourney_start_date. A date object for the date of the start of the tournament
#'   \item tourney_name. A character name for the tournament
#'   \item round. A character description of the the round the match was played in 
#'   \item opponent_name. A character name of the opponent
#'   \item win. A numeric value indicating if the player won the match 
#'   \item overall_prediction. Numeric prediction of mach outcome based on overall ("all surface" elo)
#'   \item overall_elo. Numeric overall Elo rating at end of match 
#'   \item hard_elo. Numeric hard court Elo rating at end of match 
#'   \item grass_elo. Numeric grass court Elo rating at end of match 
#'   \item clay_elo. Numeric clay court Elo rating at end of match 
#'}
#'
#' @details Only tour-level main draw matches are included in the ratings. For surface ratings, the last rating update is carried forward when matches are played on a different surface. Surface-specific ratings are NA until the first tour-level match is played on that surface.
#' @format A data frame with  298,850 rows and 11 variables
#' @name atp_elo
NULL