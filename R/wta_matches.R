#' WTA Playing Activity
#'
#' This dataset contains summaries of the outcomes of WTA matches of current and past players for the WTA World Tour. Match statistics are not available only results. The variables of the dataset are:
#'
#' \itemize{
#'   \item tourney_id. A character id that uniquely identifies each tournament
#'   \item tourney_name. A character tournament name
#'   \item surface. A character description of the court surface (Carpet, Clay, Grass, or Hard)
#'   \item draw_size. A numeric value indicating the draw size
#'   \item tourney_level. A character description of the tournament level
#'   \item tourney_start_date. The date of the first day of the tournament (not the match) as as POSIXct class
#'   \item match_num. A numeric indicating the order of matches
#'   \item winner_id. A numeric id identifying the player who won the match
#'   \item winner_seed. A numeric value for the winner's seeding
#'   \item winner_entry. A character value indicating the winner's entry type (WC = Wild card, Q = Qualifier, LL = Lucky loser, or PR = Protected ranking)
#'   \item winner_name. A character of the winner's name
#'   \item winner_hand. A character value indicated the handedness of the winner
#'   \item winner_ht. A numeric value of the winner's height in cm
#'   \item winner_ioc. A character of the winner's country of origin
#'   \item winner_age. A numeric of the winner's age at the time of the match
#'   \item winner_rank. A numeric of the winner's rank at the time of the match
#'   \item winner_rank_points. A numeric of the winner's 52-week ranking points at the time of the match
#'   \item loser_id. A numeric id identifying the player who won the match
#'   \item loser_seed. A numeric value for the loser's seeding
#'   \item loser_entry. A character value indicating the loser's entry type (WC = Wild card, Q = Qualifier, LL = Lucky loser, or PR = Protected ranking)
#'   \item loser_name. A character of the loser's name
#'   \item loser_hand. A character value indicated the handedness of the loser
#'   \item loser_ht. A numeric value of the loser's height in cm
#'   \item loser_ioc. A character of the loser's country of origin
#'   \item loser_age. A numeric of the loser's age at the time of the match
#'   \item loser_rank. A numeric of the loser's rank at the time of the match
#'   \item loser_rank_points. A numeric of the loser's 52-week ranking points at the time of the match
#'   \item score. A character of the match score
#'   \item best_of. A numeric value indicating the match format (3 or 5)
#'   \item round. A character indicating the round of the match
#'   \item W1. A numeric value for the number of games won by the winner in the first set
#'   \item W2. A numeric value for the number of games won by the winner in the second set
#'   \item W3. A numeric value for the number of games won by the winner in the third set
#'   \item W4. A numeric value for the number of games won by the winner in the fourth set
#'   \item W5. A numeric value for the number of games won by the winner in the fifth set
#'   \item L1. A numeric value for the number of games won by the loser in the first set
#'   \item L2. A numeric value for the number of games won by the loser in the second set
#'   \item L3. A numeric value for the number of games won by the loser in the third set
#'   \item L4. A numeric value for the number of games won by the loser in the fourth set
#'   \item L5. A numeric value for the number of games won by the loser in the fifth set
#'   \item Retirement. A numeric value indicating whether the match ended without completion due to retirment, W/O, or other cause
#'   \item WTB1. A numeric value for the number of points won by the winner in the first set tiebreak
#'   \item LTB1. A numeric value for the number of points won by the loser in the first set tiebreak
#'   \item WTB2. A numeric value for the number of points won by the winner in the second set tiebreak
#'   \item LTB2. A numeric value for the number of points won by the loser in the second set tiebreak
#'   \item WTB3. A numeric value for the number of points won by the winner in the third set tiebreak
#'   \item LTB3. A numeric value for the number of points won by the loser in the third set tiebreak
#'   \item WTB4. A numeric value for the number of points won by the winner in the fourth set tiebreak
#'   \item LTB4. A numeric value for the number of points won by the loser in the fourth set tiebreak
#'   \item WTB5. A numeric value for the number of points won by the winner in the fifth set tiebreak
#'   \item LTB5. A numeric value for the number of points won by the loser in the fifth set tiebreak
#'}
#'
#' @format A data frame with 102,041 rows and 70 variables
#' @source \url{https://github.com/JeffSackmann/tennis_wta}
#' @name wta_matches
NULL