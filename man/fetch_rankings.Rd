\name{fetch_rankings}
\alias{fetch_rankings}

\title{
Download ATP rankings
}
\description{
Creates a data frame of ATP rankings for a specified day.
}

\usage{
fetch_rankings(dd.mm.yyyy, min.rank = 0, max.rank = 100)
}

\arguments{
  \item{dd.mm.yyyy}{character value of the date of rankings, e.g. "01.01.2014"}
  \item{min.rank}{numeric of lowest rank to returned}
  \item{max.rank}{numeric of highest rank returned}
}


\value{

Data frame with the columns:

\describe{
  \item{player}{character of player names}
  \item{rank}{numeric rank}
  \item{points}{numeric points over previous 52 weeks}
  \item{weekchange}{numeric change in rank from previous week}
  \item{tournaments}{numeric of tournaments played in previous 52 weeks}
 }
}

\details{
ATP rankings are updated on the Monday of each week, except during Grand Slams when they are updated the Monday following the final.
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_rankings("01.01.2014", 1, 10) # Top 10 at beginning of 2014

}