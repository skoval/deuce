\name{players}
\alias{players}
\docType{data}
\title{Names and ATP codes of 200 top ATP singles players}
\description{
Data frame of 200 male ATP singles players names (ordered by rank on 5/2012). Names are as listed on ATP tour website along with corresponding code for match-specific data.
}
\usage{data(players)}

\format{
Data frame of \code{player} and \code{code} ordered by player rank
}

\source{
\url{http://www.atpworldtour.com}
}


\examples{
data(players)
players[1:10,]
}

\seealso{\link{fetch_matchfacts}}

\keyword{datasets}
\keyword{deuce}
