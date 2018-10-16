#' Detail Point-by-Point Tennis Matches
#'
#' This dataset contains detailed charting of professional tennis matches from Jeff Sackman's Match Charting Project (MCP). The variables of the dataset are:
#'
#' \itemize{
#'   \item match_id. A character id that uniquely identifies each match
#'   \item Pt. A numeric indicating the order of the point
#'   \item Set. A numeric indicating the set number (begins with 1)
#'   \item Pts. A character indicating the point status at the beginning of the point
#'   \item Gm.. A numeric indicating the game number (begins with 1)
#'   \item TbSet. A numeric indicating if a TB is used to decide a set
#'   \item TB.. A numeric indicating whether the points are doing a tiebreak
#'   \item TBpt. A numeric indicating the point order in the tiebreak
#'   \item Svr. A numeric indicating the player who is serving (1 or 2)
#'   \item Ret. A numeric indicating the player who is returning (1 or 2)
#'   \item Serving. A character indicating the initials of the player who is serving
#'   \item _1st. A character code for the outcome of first serve
#'   \item _2nd. A character code for the outcome of second serve
#'   \item Notes. A character set of notes, usually about the outcome of a challenge
#'   \item _1stNoLet. A character code for the outcome of first serve
#'   \item _2ndNoLet. A character code for the outcome of second serve
#'   \item _1stSV. 
#'   \item _2ndSV.
#'   \item _1stNoSV. 
#'   \item _2ndNoSV.
#'   \item _1stIn. 
#'   \item _2ndIn.
#'   \item isRally1st. 
#'   \item isRally2nd.
#'   \item Sv1. 
#'   \item Sv2.
#'   \item Rally.
#'   \item isAce.
#'   \item isUnret.
#'   \item isRallyWinner.
#'   \item isForced.
#'   \item isUnforced.
#'   \item isDouble.
#'   \item rallyNoSpec.
#'   \item rallyNoError.
#'   \item rallyNoDirection.
#'   \item rallyLen.
#'   \item PtWinner.
#'   \item isSvrWinner.
#'   \item PtsAfter.
#'   \item GmW.
#'   \item Gm1.1.
#'   \item Gm2.1.
#'   \item SetW.
#'   \item Set1.1.
#'   \item Set2.1.
#'   \item RevTB.
#'   \item TBrev.
#'   \item rallyCount.
#'   \item Player.1. A character with the full name of the player who served first
#'   \item Player.2. A character with the full name of the player who served second
#'   \item Pl.1.hand. A character indicating the handedness of Player 1
#'   \item Pl.2.hand. A character indicating the handedness of Player 2
#' 	  \item Gender. A character indicating the gender of the match (M = male, F = Female)
#'   \item Date. A numeric date of the tournament start, YYYYYMMDD
#'   \item Tournament. A character name of the tournament
#'   \item Round. A character indicating the round of the match (F, Q2, Q3, QF, R128, R16, R32, R64, RR, SF)
#'   \item Time. A character of the time at which the match began?
#'   \item Court. A character of the court on which the match took place 
#'   \item Surface. A character indicating the major surface used for the match
#'   \item Umpire. A character with the full name of the umpire officiating the match
#'   \item Best.of. A numeric indicating best of 5 (5) or a best of 3 (3) match
#'   \item Final.TB. A numeric indicator whether the match uses a final set tiebreak
#'   \item Charted.by. A character handle for the person who charted the match
#' 	 \item match_date. Date format of the match date
#'}
#'
#' @format A data frame with  670,790 rows and 68 variables
#' @source \url{https://github.com/JeffSackmann/tennis_MatchChartingProject}
#' @name mcp_points
NULL