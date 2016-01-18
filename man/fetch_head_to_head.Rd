\name{fetch_head_to_head}
\alias{fetch_head_to_head}

\title{
Download ATP or WTA head-to-head results
}
\description{
Creates a data frame of service and return statistics for all matches for specified player and year.
}

\usage{
fetch_head_to_head(player1, player2, type = "ATP")
}

\arguments{
   \item{player1}{character name of player}
   \item{player2}{character name of player}
   \item{type}{character either "ATP" or "WTA"}
}


\value{

Data frame with the columns:

\describe{
	\item{winner}{character name of winner}                     
	\item{years}{year of win}   
}
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{
fetch_head_to_head(player1 = "Milos Raonic", player2 = "Rafael Nadal", "ATP")
}