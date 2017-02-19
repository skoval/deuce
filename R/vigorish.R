# Implementation of Clarke's odds -> probability conversion for tennis
library(dplyr)

vig <- function(o1, o2) {
	
	O <- 1 / o1  + 1 / o2 -1
	
c(V = O / (1 + O), R = 1 - O / (1 + O))
}

find_k <- function(o1, o2){
	
	R <- vig(o1, o2)[2]
	
	k_start <- log(2 * R) / log(2)

	solver <- function(k) sum((1 / c(o1, o2)) ^ (1 / k)) - 1

uniroot(solver, c(k_start * .01, k_start * 2))$root
}


adjust_probabilities <- function(o1, o2){
	
	g <- function(o1, o2){
		
		k <- find_k(o1, o2)
		prob_k <- function(odds, k) round((1 / odds) ^ (1 / k), 2)

	data.frame(
		prob1 = prob_k(o1, k), 
		price1 = round(1 / prob_k(o1, k), 2), 
		prob2 = prob_k(o2, k),
		price2 = round(1 / prob_k(o2, k), 2)
	 )
	}
	
do.call("rbind", mapply(g, o1, o2, SIMPLIFY = FALSE))
}

example <- data.frame( 
	odds1 = c(1.80, 2.17, 2.78, 3.91, 7.05, 27.59, 49.66),
	odds2 = c(1.80, 1.54, 1.35, 1.21, 1.09, 1.02, 1.01)
	)

adjust_probabilities(example$odds1, example$odds2)