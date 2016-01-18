\name{fetch_wta_rankings}
\alias{fetch_wta_rankings}

\title{
Download WTA rankings
}
\description{
Creates a data frame of WTA rankings for a specified day.
}

\usage{
fetch_wta_rankings(dd.mm.yyyy, min.rank = 0, max.rank = 100)
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
  \item{country}{character of player's national affiliation}
  \item{points}{numeric points over previous 52 weeks}
 }
}

\details{
WTA rankings are updated on the Monday of each week, except during Grand Slams when they are updated the Monday following the final.
}

\references{
\url{http://www.wtatennis.com/singles-rankings}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_wta_rankings("01.01.2014", 1, 10) # Top 10 at beginning of 2014

}