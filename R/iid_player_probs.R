#' Generate IID Conditional Probability Inputs
#'
#' This function creates the probabilities for game, set, and match wins for the serving player given their probability of winning a point on serve
#'
#' @param pa The probability of winning on serve for player A
#' @param pb The probability of winning on serve for player B
#' @param advantage A logical value indicating whether the match is decided with an advantage set
#'
#' @export
iid_player_probs <- function(pa, pb, advantage){
	
	game_win <- function(a, b, p){
		if(a >= 4 & b >= 4){
			
			if(a - b == 2){
				a <- 4
				b <- 2
			}
			else if(b - a == 2){
				b <- 4
				a <- 2
			}
			else if(a - b == 0){
				a <- 3
				b <- 3
			}
			else if(a - b == 1){
				a <- 3
				b <- 2
			}
			else if(b - a == 1){
				b <- 3
				a <- 2
			}			
		}
		if(a == 4 & b <= 2)
			1
		else if (b == 4 & a <= 2)
			0
		else if (a == 3 & b == 3)
			p^2 / (p^2 + (1-p)^2)
		else if (a == 4 & b == 3)
			game_win(3, 2, p)
		else if (a == 3 & b == 4)
			game_win(2, 3, p)
		else
			p * game_win(a + 1, b, p) + (1 - p) * game_win(a, b + 1, p)
	}
	
	
	tiebreak_win <- function(a, b, p1, p2){ # Check calculations
		
		# Let a be the 1st player to serve
		if(a == 7 & b <= 5 | a == 8 & b == 6)
			1
		else if (b == 7 & a <= 5 | b == 8 & a == 6)
			0
		else if (a == 6 & b == 6)
			p1 * (1 - p2) / (p1 * (1 - p2) + (1 - p1) * p2)
		else if ((a + b) %% 2 == 0) # Last point on serve
			p1 * (1 - tiebreak_win(b, a + 1, p2, p1)) + (1 - p1) * (1 - tiebreak_win(b + 1, a, p2, p1))
		else
			p1 * tiebreak_win(a + 1, b, p1, p2) + (1 - p1) * tiebreak_win(a, b + 1, p1, p2)
	}
	
	set_win_tiebreak <- function(a, b, p1, p2){
		
		current.game <- a + b %% 2 # If 0 then player "a" was first to serve
		
		if(a == 6 & b <= 4 | a == 7 & b == 5)
			1
		else if(b == 6 & a <= 4 | b == 7 & a == 5)
			0
		else if(a == 6 & b == 6)
			if(current.game == 0)
				tiebreak_win(0, 0, p1, p2) # Player who served first in set serves first in tiebreak
			else
				1 - tiebreak_win(0, 0, p2, p1) # Other player serves
		else
			game_win(0, 0, p1) * ( 1 - set_win_tiebreak(b, a + 1, p2, p1)) + (1 - game_win(0, 0, p1)) * (1 - set_win_tiebreak(b + 1, a, p2, p1))
				
	}
	
	
	
	set_win_advantage <- function(a, b, p1, p2){
		
		if(a >= 5 & b >= 5){
			
			if(a - b == 1){
				a <- 5
				b <- 4
			}
			else if(b - a == 1){
				a <- 4
				b <- 5
			}
			else if(a - b == 0){
				a <- 5
				b <- 5
			}
			
		}
		
		if(a == 6 & b <= 4)
			1
		else if(b == 6 & a <= 4)
			0
		else if(a == 5 & b == 5){
			g1 <- game_win(0, 0, p1)
			g2 <- game_win(0, 0, p2)
			g1 * (1 - g2) / (g1 * (1 - g2) + (1 - g1) * g2 )
			}
		else
			game_win(0, 0, p1) * ( 1 - set_win_advantage(b, a + 1, p2, p1)) + (1 - game_win(0, 0, p1)) * (1 - set_win_advantage(b + 1, a, p2, p1))
				
	}
	
	match_win_5 <- function(p1, p2, advantage){
			
				X <- matrix(rbinom(n = 1000000, size = 1, set_win_tiebreak(0, 0, p1, p2)), ncol = 5)
				
				M <- matrix(0, 4, 4)
				
				M[4,] <- 1
				
				M[4, 4] <- NA
				
				M[1, 1] <- mean(apply(X, 1, function(x){
					if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}))
		
				M[1, 2] <- mean(apply(X, 1, function(x){
					if(x[1] == 1) # Won first set
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)
		
				M[2, 1] <- mean(apply(X, 1, function(x){
					if(x[1] == 0) # Lost first set
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)
				
				
				M[2, 2] <- mean(apply(X, 1, function(x){
					if(sum(x[1:2]) != 1) # Split first two
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)	
				
				M[1, 3] <- mean(apply(X, 1, function(x){
					if(sum(x[1:2]) != 0) # Lost first two
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)	
				
				M[3, 1] <- mean(apply(X, 1, function(x){
					if(sum(x[1:2]) != 2) # Won first two
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)
		
		
				M[3, 2] <- mean(apply(X, 1, function(x){
					if(sum(x[1:3]) != 2) # 2-1
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)	
				
				
				 M[2, 3] <- mean(apply(X, 1, function(x){
					if(sum(x[1:3]) != 1) # 2-1
						NA
					else if(!any(cumsum(x) == 3))
						FALSE
					else if(!any(cumsum(1 - x) == 3))
						TRUE
					else
						min(which(cumsum(x) == 3)) < min(which(cumsum(1 - x) == 3))
				}), na.rm = TRUE)	
				
				if(advantage)
				 	M[3, 3] <- set_win_advantage(0, 0, p1, p2)
				else
				 	M[3, 3] <- set_win_tiebreak(0, 0, p1, p2)
				 	
	M						
	}
	
	
	
	match_win_advantage <- function(a, b, advantage, bestof3, M = M){
				
		if(bestof3)
			M <- M[2:4, 2:4]
	
	M[(a+1), (b+1)]
	}

	point_score <- matrix(0, 5, 5)
	point_grid <- expand.grid(0:3, 0:3)
	
	gameA <- mapply(game_win, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p = pa))
	gameB <- mapply(game_win, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p = pb))
	
	gameA_mat <- point_score
	gameA_mat[1:4, 1:4] <- matrix(gameA, 4, 4)
	
	gameB_mat <- point_score
	gameB_mat[1:4, 1:4] <- matrix(gameB, 4, 4)
	
	gameA_mat[nrow(gameA_mat), 1:4] <- 1
	gameB_mat[nrow(gameB_mat), 1:4] <- 1
	
	point_score <- matrix(0, 8, 8)
	point_grid <- expand.grid(0:6, 0:6)
	
	tbgameA <- mapply(tiebreak_win, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pa, p2 = pb))
	tbgameB <- mapply(tiebreak_win, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pb, p2 = pa))
	
	tbgameA_mat <- point_score
	tbgameA_mat[1:7, 1:7] <- matrix(tbgameA, 7, 7)
	
	tbgameB_mat <- point_score
	tbgameB_mat[1:7, 1:7] <- matrix(tbgameB, 7, 7)
	
	tbgameA_mat[nrow(tbgameA_mat), 1:7] <- 1
	tbgameB_mat[nrow(tbgameB_mat), 1:7] <- 1
	
	point_score <- matrix(0, 7, 7)
	point_grid <- expand.grid(0:6, 0:6)
	
	settbgameA <- mapply(set_win_tiebreak, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pa, p2 = pb))
	settbgameB <- mapply(set_win_tiebreak, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pb, p2 = pa))
	
	settbgameA_mat <- point_score
	settbgameA_mat[1:7, 1:7] <- matrix(settbgameA, 7, 7)
	
	settbgameB_mat <- point_score
	settbgameB_mat[1:7, 1:7] <- matrix(settbgameB, 7, 7)
		
	setadvgameA <- mapply(set_win_advantage, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pa, p2 = pb))
	setadvgameB <- mapply(set_win_advantage, a =  point_grid[,1],  b =  point_grid[,2], MoreArgs = list(p1 = pb, p2 = pa))
	
	setadvgameA_mat <- settbgameA_mat
	setadvgameA_mat[1:7, 1:7] <- matrix(setadvgameA, 7, 7)
	
	setadvgameB_mat <- settbgameB_mat
	setadvgameB_mat[1:7, 1:7] <- matrix(setadvgameB, 7, 7)
			
	MA <- match_win_5(pa, pb, TRUE)
	MB <- 1 - t(MA)
			
list(A = list(game = gameA_mat, tiebreak = tbgameA_mat, set_tiebreak = settbgameA_mat, set_advantage = setadvgameA_mat, match = MA), 
	B = list(game = gameB_mat, tiebreak = tbgameB_mat, set_tiebreak = settbgameB_mat, set_advantage = setadvgameB_mat, match = MB)
	)
}

