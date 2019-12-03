#' Calculate Loss Importance for a Point
#'
#' This function calculates the match win prediction for matches in progress based on the current score
#'
#' @param id Score id for the start of the current point
#' @param server.prob Numeric serve win prob of current server
#' @param return.prob Numeric serve win prob of current returner
#' @param bestof3 Logical indicator if best-of-3 match (TRUE) or best-of-5 (FALSE)
#' @param advantage Logical if advantage set match (TRUE) or tiebreak match (FALSE)
#'
#' @return Numeric value of importance
#' @export
serve_loss_importance <- function (id, serve.prob, return.prob, bestof3 = TRUE, advantage = TRUE) 
{
	
	# Win from the current point
	won_id <- update_score(
		as.numeric(substr(id, 1, 1)),
		as.numeric(substr(id, 2, 2)),
		as.numeric(substr(id, 3, 3)),
		as.numeric(substr(id, 4, 4)),
		as.numeric(substr(id, 5, 5)),
		as.numeric(substr(id, 6, 6)),
		advantage,
		bestof3
	)
	
	loss_id <- update_score(
		as.numeric(substr(id, 1, 1)),
		as.numeric(substr(id, 2, 2)) + 1,
		as.numeric(substr(id, 3, 3)),
		as.numeric(substr(id, 4, 4)),
		as.numeric(substr(id, 5, 5)),
		as.numeric(substr(id, 6, 6)),
		advantage,
		bestof3
	)	
	
	if(substr(won_id, 7, 7) == "F"){
		win <- in_match_win(
			as.numeric(substr(won_id, 1, 1)),
			as.numeric(substr(won_id, 2, 2)),
			as.numeric(substr(won_id, 3, 3)),
			as.numeric(substr(won_id, 4, 4)),
			as.numeric(substr(won_id, 5, 5)),
			as.numeric(substr(won_id, 6, 6)),
			serve.prob,
			return.prob,
			bestof3,
			advantage
		)
	}
	else{
		win <- in_match_win(
			as.numeric(substr(won_id, 1, 1)),
			as.numeric(substr(won_id, 2, 2)),
			as.numeric(substr(won_id, 3, 3)),
			as.numeric(substr(won_id, 4, 4)),
			as.numeric(substr(won_id, 5, 5)),
			as.numeric(substr(won_id, 6, 6)),
			return.prob,
			serve.prob,
			bestof3,
			advantage
		)	
		win <- 1 - win	
	}
	
	
	if(substr(loss_id, 7, 7) == "F"){
		lost <- in_match_win(
			as.numeric(substr(loss_id, 1, 1)),
			as.numeric(substr(loss_id, 2, 2)),
			as.numeric(substr(loss_id, 3, 3)),
			as.numeric(substr(loss_id, 4, 4)),
			as.numeric(substr(loss_id, 5, 5)),
			as.numeric(substr(loss_id, 6, 6)),
			serve.prob,
			return.prob,
			bestof3,
			advantage
		)
	}
	else{
		lost <- in_match_win(
			as.numeric(substr(loss_id, 1, 1)),
			as.numeric(substr(loss_id, 2, 2)),
			as.numeric(substr(loss_id, 3, 3)),
			as.numeric(substr(loss_id, 4, 4)),
			as.numeric(substr(loss_id, 5, 5)),
			as.numeric(substr(loss_id, 6, 6)),
			return.prob,
			serve.prob,
			bestof3,
			advantage
		)		
		
		lost <- 1 - lost
	}	
	
win - lost
}


serve_loss_importance <- Vectorize(serve_loss_importance)