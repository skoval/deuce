\name{fetch_wta_draw}
\alias{fetch_wta_draw}

\title{
Download Grand Slam tournament results for the WTA.
}
\description{
Creates a data frame of draw and results of Grand Slam tournaments for the WTA.
}

\usage{
fetch_wta_draw(tournament, year)
}

\arguments{
  \item{tournament}{character of tournament name keyword}
  \item{year}{numeric year}
}


\value{

Data frame with the columns:

\describe{
\item{winner}{character of player names}    
\item{loser}{character of player names} 
\item{round}{numeric of round of match (1 = first round, 2 = second round, etc.)}                   
\item{year}{numeric}              
\item{date}{date object of date, see \code{\link{ymd}}}      
  }
}

\references{
\url{http://www.stevegtennis.com/draw-results/wta}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{
fetch_wta_draw(tournament = "Wimbledon", year = 2013)
}