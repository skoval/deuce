game_win <- function(a, b, p){
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


point_importance <- function(a, b, p){
	game_win(a + 1, b, p) - game_win(a, b + 1, p)
}

point_clutch <- function(a, b, p){
 1 - game_win(a, b + 1, p) # Loss
}

point_closing <- function(a, b, p){
 game_win(a + 1, b, p) # win
}