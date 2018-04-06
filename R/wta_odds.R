#' WTA Match Odds
#'
#' This dataset contains outcomes of matches on the WTA Tour from 2007 forward along with betting odds. The variables of the dataset are:
#'
#' \itemize{
#'   \item WTA. A numeric value for the tournament number
#'   \item Location. A character venue of tournament
#'   \item Tournament. A character name of tounament (including sponsor if relevant)
#'   \item Date. A date object for the date of match 
#'   \item Tier. A character name of tier (tournament ranking).
#'   \item Court. A character indicating the type of court (Outdoor or Indoor)
#'   \item Surface. A character indicating the type of surface (Clay, Hard, Carpet or Grass)
#'   \item Round. A character indicating the round of match
#'   \item Best.of. A numeric with the maximum number of sets playable in match
#'   \item Winner. A character (Last First Initial) name of the match winner
#'   \item Loser. A character (Last First Initial) name of the match loser
#'   \item WRank. A character WTA Entry ranking of the match winner as of the start of the tournament
#'   \item LRank. A character WTA Entry ranking of the match loser as of the start of the tournament
#'   \item WPts. A character WTA Entry points of the match winner as of the start of the tournament
#'   \item LPts. A character WTA Entry points of the match loser as of the start of the tournament
#'   \item W1. A numeric Number of games won in 1st set by match winner
#'   \item L1. A numeric Number of games won in 1st set by match loser
#'   \item W2. A numeric Number of games won in 2nd set by match winner
#'   \item L2. A numeric Number of games won in 2nd set by match loser
#'   \item W3. A numeric Number of games won in 3rd set by match winner
#'   \item L3. A numeric Number of games won in 3rd set by match loser
#'   \item Wsets. A numeric Number of sets won by match winner
#'   \item Lsets. A numeric Number of sets won by match loser
#'   \item Comment. A character Comment on the match (Completed, won through retirement of loser, or via Walkover)
#'   \item B365W. A numeric Bet365 odds of match winner
#'   \item B365L. A numeric Bet365 odds of match loser
#'   \item CBW. A numeric Centrebet odds of match winner
#'   \item CBL. A numeric Centrebet odds of match loser
#'   \item EXW. A numeric Expekt odds of match winner
#'   \item EXL. A numeric Expekt odds of match loser
#'   \item PSW. A numeric Pinnacles Sports odds of match winner
#'   \item PSL. A numeric Pinnacles Sports odds of match loser
#'   \item UBW. A numeric Unibet odds of match winner
#'   \item UBL. A numeric Unibet odds of match loser
#'   \item LBW. A numeric Ladbrokes odds of match winner
#'   \item LBL. A numeric Ladbrokes odds of match loser
#'   \item SJW. A numeric Stan James odds of match winner
#'   \item SJL. A numeric Stan James odds of match loser
#'   \item MaxW. A numeric Maximum odds of match winner (as shown by Oddsportal.com)
#'   \item MaxL. A numeric Maximum odds of match loser (as shown by Oddsportal.com)
#'   \item AvgW. A numeric Average odds of match winner (as shown by Oddsportal.com)
#'   \item AvgL. A numeric Average odds of match loser (as shown by Oddsportal.com)
#'   \item url. A character with the url page for the tournament CSV files
#'   \item Year. Numeric value of the year of the match
#'   \item id. Character id for the match
#'}
#'
#' @format A data frame with 27,784 rows and 45 variables
#' @source \url{http://www.tennis-data.co.uk/alldata.php}
#' @name wta_odds
NULL