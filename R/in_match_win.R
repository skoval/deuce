#' Estimating Conditional Match Win
#'
#' This function uses IID formula to estimate the conditional probability that the player who is currently serving wins the match given the match format and current score
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
in_match_win <- function(point_a, point_b, game_a, game_b, set_a, set_b, serving_player = 1, matrices, bestof3 = TRUE, advantage = TRUE){

	# Win is with respect to the player who is currently serving
	# 'a' is serving player score and 'b' is returning player score
	
	max.sets <- ifelse(bestof3, 3, 5)
	
	is.tiebreak <- (game_a == 6 & game_b == 6 & advantage & set_a + set_b + 1 != max.sets) | (game_a == 6 & game_b == 6 & !advantage)
		
	returning_player <- ifelse(serving_player == 1, 2, 1)
	
	if(max.sets == 3){
		matrices[[1]]$match <- matrices[[1]]$match[2:nrow(matrices[[1]]$match), 2:nrow(matrices[[1]]$match)]
		matrices[[2]]$match <- matrices[[2]]$match[2:nrow(matrices[[2]]$match), 2:nrow(matrices[[2]]$match)]
	}
	
	win_loss <- function(win_game = TRUE, win_set = TRUE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = 3, advantage, is.tiebreak, serving_player, returning_player, matrices){
	
		
		# Chance of winning current game
		# Check for plausible value
		
		winning_game <- function(point_a, point_b, win_game, is.tiebreak, matrices){

			if(is.tiebreak & win_game){
				if(point_a + point_b %% 4 %in% c(0, 3))
					part1 <-  matrices[[serving_player]]$tiebreak[(point_a + 1),(point_b + 1)]
				else 
					part1 <-  1 - matrices[[returning_player]]$tiebreak[(point_b + 1),(point_a + 1)]
			}
			else if(!is.tiebreak & win_game)
			 	 part1 <- matrices[[serving_player]]$game[(point_a + 1),(point_b + 1)] 
			else if(is.tiebreak & !win_game){
				if(point_a + point_b %% 4 %in% c(0, 3))
					part1 <-  1 - matrices[[serving_player]]$tiebreak[(point_a + 1),(point_b + 1)]
				else 
					part1 <-  matrices[[returning_player]]$tiebreak[(point_b + 1),(point_a + 1)]
			}
			else
				part1 <- 1 - matrices[[serving_player]]$game[(point_a + 1),(point_b + 1)] 
			
			part1
		}
		
		winning_set <- function(game_a, game_b, win_set, is.tiebreak, advantage, matrices, returning_player, max.sets){
			if(is.tiebreak){
				part2 <- 1
			}
			else if(win_set){
				if(!advantage | advantage & set_a + set_b + 1 != max.sets)
					part2 <- 1 - matrices[[returning_player]]$set_tiebreak[(game_b + 1),(game_a + 1)]
				else
					part2 <- 1 - matrices[[returning_player]]$set_advantage[(game_b + 1),(game_a + 1)]
			}
			else{
				if(!advantage | advantage & set_a + set_b + 1 != max.sets)
					part2 <- matrices[[returning_player]]$set_tiebreak[(game_b + 1),(game_a + 1)]
				else
					part2 <- matrices[[returning_player]]$set_advantage[(game_b + 1),(game_a + 1)]
			}		
	
		part2
		}
		
		winning_match <- function(set_a, set_b, max.sets, serving_player){
			if(max.sets == 3 & set_a >= 2 & set_b <= 1 | max.sets == 5 & set_a >= 3 & set_b <= 2)
				part3 <- 1
			else if (max.sets == 3 & set_b >= 2 & set_a <= 1 | max.sets == 5 & set_b >= 3 & set_a <= 2)
				part3 <- 0
			else 
				part3 <- matrices[[serving_player]]$match[(set_a + 1),(set_b + 1)]	
		part3 
		}
		
		
		part1 <- winning_game(point_a = point_a, point_b = point_b, win_game = win_game, is.tiebreak = is.tiebreak, matrices = matrices)
				
		if(win_game & win_set & game_a == 6 & game_b <= 5){
			part2 <- 1
			part3 <- winning_match(set_a = set_a + 1, set_b = set_b, max.sets = max.sets, serving_player = serving_player)
		}
		else if(win_game & !win_set & game_a == 6 & game_b <= 5){
			part2 <- 0 # Not possible
			part3 <- 0
		}
		else if(!win_game & win_set & game_b == 6 & game_a <= 5){
			part2 <- 0 # Not possible
			part3 <- 0
		}
		else if(!win_game & !win_set & game_b == 6 & game_a <= 5){
			part2 <- 1 # Winning from a set down after losing game
			part3 <- winning_match(set_a = set_a, set_b = set_b + 1, max.sets = max.sets, serving_player = serving_player)
		}
		else if(win_game){
			part2 <- winning_set(game_a = game_a + 1, game_b = game_b, win_set = win_set, is.tiebreak = is.tiebreak, advantage = advantage, matrices = matrices, returning_player = returning_player, max.sets = max.sets)
			if(win_set)
				part3 <- winning_match(set_a = set_a + 1, set_b = set_b, max.sets = max.sets, serving_player = serving_player)
		else
				part3 <-  winning_match(set_a = set_a , set_b = set_b + 1, max.sets = max.sets, serving_player = serving_player)

			}
		else{
			part2 <- winning_set(game_a = game_a, game_b = game_b + 1, win_set = win_set, is.tiebreak = is.tiebreak, advantage = advantage, matrices = matrices, returning_player = returning_player, max.sets = max.sets)
				
		if(win_set)
			part3 <- winning_match(set_a = set_a + 1, set_b = set_b, max.sets = max.sets, serving_player = serving_player)
		else
			part3 <-  winning_match(set_a = set_a , set_b = set_b + 1, max.sets = max.sets, serving_player = serving_player)

	}
	
	#print(c(part1, part2, part3))	
	
	part1 * part2 * part3		
 	}
 	
 	if(is.tiebreak){
 		
 		type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = TRUE)
 		
 		type3 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = TRUE)
 		
 	type1 + type3
 	}
 	else{
 		
 		# Game and set are won
  		type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = is.tiebreak)
 		
 		type3 <- win_loss(TRUE, FALSE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = is.tiebreak)
 			
  		type2 <- win_loss(FALSE, TRUE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = is.tiebreak)
 		
 		type4 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, game_b, set_a, set_b, max.sets = max.sets, serving_player = serving_player, returning_player = returning_player, matrices = matrices, advantage = advantage, is.tiebreak = is.tiebreak)
		
		print(c(type1, type2, type3, type4))
 	
 	type1 + type2 + type3 + type4
 	}

}
