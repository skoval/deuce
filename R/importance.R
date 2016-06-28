#' Calculate Importance of Point
#'
#' Calculates the importance of a point using Carl Morris' definition of importance. This is the difference in the probability of winning a match when the current point is won compared to when it is lost.
#'
#' @param point_a The current point score within a game for player A (numerical; 1 = 15, 2 = 30, etc.)
#' @param point_b The current point score within a game for player B 
#' @param game_a The current game score within a game for player A
#' @param game_b The current game score within a game for player B 
#' @param set_a The current set score within a game for player A 
#' @param set_b The current set score within a game for player B 
#' @param serving_player Numerical (1 or 2) if first element in \code{matrices} is the serving player = 1; 2 otherwise
#' @param matrices List of IID inputs from \code{iid_player_probs}
#' @param bestof3 Logical indicator if a best-of-3 match
#' @param advantage Logical indicator if an advantage match
#'
#' @export
importance <- function(point_a, point_b, game_a, game_b, set_a, set_b, serving_player = 1, matrices, bestof3 = TRUE, advantage = TRUE){

	update_score <- function(pointa, pointb, gamea, gameb, seta, setb, advantage, bestof3){
		
		tiebreak.set <- !advantage | (seta + setb + 1) != ifelse(bestof3, 3, 5)
		is.tiebreak <- gamea + gameb == 12 & (!advantage | (seta + setb + 1) != ifelse(bestof3, 3, 5))
		serve.changed <- FALSE
		
		is.game.won <- function(pointa, pointb, is.tiebreak){
			
			if(is.tiebreak)
				pointa >= 7 & pointb <= 5
			else
				pointa >= 4 & pointb <= 2
		}
		
		is.set.won <- function(gamea, gameb, tiebreak.set){
			
			if(tiebreak.set)
				gamea == 6 & gameb <= 4 | gamea == 7 & gameb <= 6 
			else
				gamea == 6 & gameb <= 4 | gamea == 7 & gameb <= 5
		}
		
			
		if(is.game.won(pointa, pointb, is.tiebreak)){
			pointa <- 0
			pointb <- 0
			gamea <- gamea + 1
			serve.changed <- TRUE
		}
		
		if(pointa == 7 & pointb == 6 & is.tiebreak){
			pointa <- 6
			pointb <- 5
		}

		if(pointa == 6 & pointb == 7 & is.tiebreak){
			pointa <- 5
			pointb <- 6
		}

		if(pointa == 4 & pointb == 3 & !is.tiebreak){
			pointa <- 3
			pointb <- 2
		}

		if(pointa == 3 & pointb == 4 & !is.tiebreak){
			pointa <- 2
			pointb <- 3
		}
						
		if(is.game.won(pointb, pointa, is.tiebreak)){
			pointa <- 0
			pointb <- 0
			gameb <- gameb + 1
			serve.changed <- TRUE
		}
		
			
		if(is.set.won(gamea, gameb, tiebreak.set)){
			gamea <- 0
			gameb <- 0
			seta <- seta + 1
		}
			
		if(is.set.won(gameb, gamea, tiebreak.set)){
			gamea <- 0
			gameb <- 0
			setb <- setb + 1
		}	
		
		
		data.frame(
			pointa = pointa, 
			pointb = pointb, 
			gamea = gamea, 
			gameb = gameb, 
			seta = seta,
			setb = setb,
			serve.changed = serve.changed
		)
	}
	
	is.match.won <- function(seta, setb, bestof3){
		
		if(bestof3)
			seta == 2 & setb <= 1
		else
			seta == 3 & setb <= 2
	}
		
	winning_score <- update_score(point_a + 1, point_b, game_a, game_b, set_a, set_b, advantage, bestof3)
	losing_score <- update_score(point_a, point_b + 1, game_a, game_b, set_a, set_b, advantage, bestof3)
		
	#print(winning_score)
	#print(losing_score)
	
	if(is.match.won(winning_score$seta, winning_score$setb, bestof3))
		win <- 1
	else 
		if(winning_score$serve.changed)
	     	win <- 1 - in_match_win(winning_score$pointb, winning_score$pointa, winning_score$gameb, winning_score$gamea, winning_score$setb, winning_score$seta, serving_player = ifelse(serving_player == 1, 2, 1), matrices = matrices, bestof3 = bestof3, advantage = advantage)
		else
	     	win <- in_match_win(winning_score$pointa, winning_score$pointb, winning_score$gamea, winning_score$gameb, winning_score$seta, winning_score$setb, serving_player = serving_player, matrices = matrices, bestof3 = bestof3, advantage = advantage)
		
		
	if(is.match.won(losing_score$setb, losing_score$seta, bestof3))
		loss <- 0
	else
		if(losing_score$serve.changed)
	     	loss <- 1 - in_match_win(losing_score$pointb, losing_score$pointa, losing_score$gameb, losing_score$gamea, losing_score$setb, losing_score$seta, serving_player = ifelse(serving_player == 1, 2, 1), matrices = matrices, bestof3 = bestof3, advantage = advantage)
		else
	     	loss <- in_match_win(losing_score$pointa, losing_score$pointb, losing_score$gamea, losing_score$gameb, losing_score$seta, losing_score$setb, serving_player = serving_player, matrices = matrices, bestof3 = bestof3, advantage = advantage)
		
		

data.frame(win = win, loss = loss, importance = win - loss)
}