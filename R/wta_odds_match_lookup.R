#' WTA Match Odds Lookup Table
#'
#' This dataset contains the match id and corresponding player and match id from the \code{wta_matches}.
#'
#' \itemize{
#'   \item id. Character id for the match
#'   \item winner_id. Character id for the winning player as in \code{wta_matches}.
#'   \item loser_id. Character id for the losing player as in \code{wta_matches}.
#'   \item tourney_id. Character id for the tournament as in \code{wta_matches}.
#'   \item match_id. Character id for the match id as in \code{wta_matches}.
#'}
#'
#' @details If \code{match_id} is NA it means a certain match could not be found
#' @format A data frame with 27,784 rows and 5 variables
#' @name wta_odds_match_lookup
NULL