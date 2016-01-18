#' Match Win Probability
#'
#' Match win probability under an IID model
#'
#' @param serve probability that favored player wins a point on serve
#' @param return probability that favored player wins a point on return
#' @param bestof3 logical indicator whether best-of-3 match (TRUE) or best-of-5 (FALSE)
#'
#' @export
match_win <- function(serve, return, bestof3 = TRUE){
	
		p <- serve
		q <- return
		
		A <- c(1, 3,0,4,0,0, 
			  3, 3,1,4,0,0,
			4, 4,0,3,1,0, 
			6, 3,2,4,0,0,
			16, 4,1,3,1,0,
			6, 5,0,2,2,0, 
			10, 2,3,5,0,0,
			40, 3,2,4,1,0, 
			30, 4,1,3,2,0,
			4, 5,0,2,3,0, 
			5, 1,4,6,0,0,
			50, 2,3,5,1,0, 
			100, 3,2 ,4, 2,0,
			50, 4,1,3,3,0, 
			5, 5,0,2,4,0, 
			1, 1,5,6,0,0,
			30, 2,4,5,1,0, 
			150, 3,3, 4,2,0,
			200, 4,2, 3, 3,0, 
			75, 5,1,2,4,0,
			6, 6,0,1,5,0,
			1, 0,6,6,0,1, 
			36, 1,5,5,1,1,
			225,2,4, 4, 2,1, 
			400, 3,3, 3, 3,1,
			225,4,2, 2, 4,1, 
			36, 5,1,1,5,1,
			1, 6,0,0,6,1)
	
	A <- matrix(A, byrow = TRUE, ncol = 6)
	
	B <- c(1, 3, 0, 3, 0, 0, 
		3, 3, 1, 3, 0, 0,
		3, 4, 0, 2, 1, 0, 
		6, 2, 2, 4, 0, 0,
		12, 3, 1, 3, 1, 0, 
		3, 4, 0, 2, 2, 0,
		4, 2, 3, 4, 0, 0, 
		24, 3, 2, 3, 1, 0,
		24, 4, 1, 2, 2, 0,
		4, 5, 0, 1, 3, 0,
		5, 1, 4, 5, 0, 0, 
		40, 2, 3, 4, 1, 0,
		60, 3, 2, 3, 2, 0,
		20, 4, 1,2, 3, 0,
		1, 5, 0, 1, 4, 0,
		1, 0, 5, 5, 0, 1, 
		25, 1, 4, 4, 1, 1, 
		100, 2, 3, 3, 2, 1, 
		100, 3, 2, 2, 3, 1,
		25, 4, 1, 1, 4, 1,
		1, 5, 0, 0, 5, 1)
		
	B <- matrix(B, byrow = TRUE, ncol = 6)

	G <- function(x) x^4 * (15 - 4*x - (10 *x^2)/(1-2*x*(1-x)))
	
	tb <- function(x, p, q){
		D <- p*q/(1 - (p*(1-q)+(1-p)*q))
		A[x, 1] * p^A[x, 2] * (1-p)^A[x, 3] * q^A[x, 4] *(1-q)^A[x, 5] * D^A[x, 6]
	}
	
	s <- function(x, p, q){
		B[x, 1] * G(p)^B[x, 2] * (1-G(p))^B[x, 3] * G(q)^B[x,4]	* (1-G(q))^B[x, 5] *
		(G(p) * G(q) + (G(p)*(1-G(q)) + G(q) * (1-G(p))) * TB(p, q) )^B[x, 6]
	}
	
	TB <- function(p, q) sum(sapply(1:28, tb, p = p, q = q))
	S <- function(p, q) sum(sapply(1:21, s, p = p, q = q))
	
	if(bestof3)
		match_win_prob <- S(p, q)^2 * (1 + 2 * (1 - S(p, q)))
	else
		match_win_prob <-  S(p, q)^3 * (1 + 3 * (1 - S(p, q)) + 6  * (1 - S(p, q))^2)
		
ifelse(match_win_prob < 0, 0, ifelse(match_win_prob > 1, 1, match_win_prob))
}