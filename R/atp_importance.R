#' ATP Point Importance
#'
#' A dataset with point importance for all possible scorelines in an ATP match:
#'
#' \itemize{
#'   \item serve_point. Numeric points won by server in game
#'   \item return_point. Numeric points won by returner in game
#'   \item serve_game. Numeric games won by server in set
#'   \item return_game. Numeric games won by returner in set
#'   \item serve_set. Numeric sets won by server in match
#'   \item return_set. Numeric sets won by returner in match
#'   \item id. Character score id that collapses point, game, and set scoreline.
#'   \item importance. Numeric importance of point
#'   \item serve_score_name. Nominal point score for server
#'   \item return_score_name. Nominal point score for returner
#'   \item game_score. Nominal game score
#'   \item set_score. Nominal set score
#'   \item bestof3. Numeric indicator of match format, 3 for best of 3 and 5 for best of 5.
#'}
#' @details The match format supposes a final set tiebreak. Both players are supposes to have average skill and win 65% of points on serve.
#' @format A data frame with 8,749 rows and 14 variables
#' 	@references O'Donoghue, P. G. (2001). The most important points in grand slam singles tennis. Research quarterly for exercise and sport, 72(2), 125-131.
#' @name atp_importance
NULL