#' Update Elo Ratings
#'
#' This function updates player Elo ratings given the outcome of a match
#'
#' @param elo A two-element vector of player Elo ratings at the start of a match
#' @param match A two-element vector of the career matches played by each player
#' @param win A two-element vector with a 1 for the winner and 0 for the loser
#' @param slam A logical indicator of whether the match was a Grand Slam
#'
#' @export
update_elo <- function(elo, match, win, slam = TRUE){
	
	rating_diff <- diff(elo)
	
	prediction <- 1 / (1 + 10^(c(1, -1) * rating_diff / 400))
	
	k <- 250 / ((match + 5) ^ 0.4)
	k <- ifelse(slam, 1.1 * k, k)
	
	new_elo <- elo + k * (win - prediction)
	
data.frame(
	predict = prediction,
	elo = new_elo,
	matches = match + 1
)
}

