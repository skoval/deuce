#' Update Score
#'
#' This function updates the score if game, set, or match won
#'
#' @param pointa Points won by server
#' @param pointb Points won by receiver
#' @param gamea Games won by server
#' @param gameb Games won by receiver
#' @param seta Sets won by server
#' @param setb sets won by receiver
#' @param advantage Indicator (T or F) if advantage final set
#' @param bestof3 Indicator (T or F) if best of 3 match or best of 5 match
#'
#' @return Match win probability for serving player
update_score <- function(pointa, pointb, gamea, gameb, seta, setb, advantage, bestof3){
		
		tiebreak.set <- !advantage | (seta + setb + 1) != ifelse(bestof3, 3, 5)
		
		is.tiebreak <- (gamea + gameb) == 12 & (!advantage | (seta + setb + 1) != ifelse(bestof3, 3, 5))
		
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
		
if(serve.changed)
	paste0(pointb, pointa, gameb, gamea, setb, seta, "T")
else
	paste0(pointa, pointb, gamea, gameb, seta, setb, "F")
}

update_score <- Vectorize(update_score)