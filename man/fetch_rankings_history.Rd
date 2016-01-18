\name{fetch_rankings_history}
\alias{fetch_rankings_history}

\title{
Download ATP player's singles rankings history
}
\description{
Creates a data frame of the ATP singles rankings for each week of a player's career.
}

\usage{
fetch_rankings_history(player)
}

\arguments{
  \item{player}{character value of player name (partial matching attempted)}
}


\value{

Data frame with the columns:

\describe{
  \item{Day}{days as numeric}
  \item{Month}{month as numeric}
  \item{Year}{year as numeric}
  \item{Rank}{numeric rank}
  \item{Date}{dates as Date object}
 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_rankings_history("Pete Sampras")

}