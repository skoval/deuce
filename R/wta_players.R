#' Biographic Details of WTA Players
#'
#' This dataset contains the names and basic demographic details of current and past players for the WTA World Tour. The variables of the dataset are:
#'
#' \itemize{
#'   \item player_id. A numeric id to link players to other datasets
#'   \item first_name. A character vector of player first names
#'   \item last_name. A character vector of player last names
#'   \item player_id. A numeric id to link players to other datasets
#'   \item hand. A character indicating handedness type: L = left, R = right, U = unknown, A = ambidextrous
#'   \item birthdate. A numeric birthdate, YYYYYMMDD
#'   \item country_code. A character of 3-letter codes for country
#'	 \item invalid. Logical if whether player names has any blanks or missing values.
#'   \item name. A character of player full names, first name + last name
#'   \item dob. Date of birth as POSIXct class
#' }
#'
#' @format A data frame with 21,340 rows and 9 variables
#' @source \url{https://github.com/JeffSackmann/tennis_wta}
#' @name wta_players
NULL