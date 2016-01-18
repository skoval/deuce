\name{tournaments}
\alias{tournaments}
\docType{data}
\title{Names and codes for ATP tournaments}
\description{
Data frame of ATP tournaments at the 250 level and above. 
}
\usage{data(tournaments)}

\format{
Data frame of \code{code}, \code{location}, \code{tournament}, \code{tier}, \code{rounds} and \code{surface}.
}

\source{
\url{http://www.atpworldtour.com}
}


\examples{
data(tournaments)
tournaments[1:10,]
}

\seealso{\link{fetch_draw}}

\keyword{datasets}
\keyword{deuce}
