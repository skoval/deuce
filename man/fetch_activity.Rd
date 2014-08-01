\name{fetch_activity}
\alias{fetch_activity}

\title{
Download ATP player's singles playing activity for a specified year
}
\description{
Creates a data frame of the date, tournament played, and result for ATP singles play during a given year.
}

\usage{
fetch_activity(player, year = NULL)
}

\arguments{
  \item{player}{character value of keyword for player name}
  \item{year}{numeric year of tournament play, if \code{NULL} all years are returned}
}


\value{

Data frame with the columns:

\describe{
 \item{day}{days as numeric}
 \item{month}{months as numeric}
  \item{year}{years as numeric}
  \item{date}{dates as Date}
  \item{tournament}{character name of tournament}
  \item{result}{character description of result}
  \item{wins}{matches won in tournament excluding byes}
   \item{losses}{matches lost in tournament}
  \item{surface}{character description of tournament surface}
 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_activity("Wilander", 1996)

}