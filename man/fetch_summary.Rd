\name{fetch_summary}
\alias{fetch_summary}

\title{
Download ATP player match statistics
}
\description{
Creates a data frame of match statistics for a specified list of players based on the current data provided on the ATP tour website.
}
\usage{
fetch_summary(players, year = NULL, surface = c("All", "Clay", "Hard", "Grass"))
}

\arguments{
  \item{players}{vector of player names (partial matching attempted)}
  \item{year}{numeric indicating year or \code{NULL} for career stats}
  \item{surface}{character indicating surface for match summary statistics}
}


\value{

Data frame with the columns:

\describe{
\item{Aces}{total number of aces}
\item{Double Faults}{total number of double faults}
\item{1st Serve}{percentage of first serves in}
\item{1st Serve Points Won}{percentage of first service points won}
\item{2nd Serve Points Won}{percentage of first service point won}
\item{Break Points Faced}{number of times opponent had break point opportunity against player}
\item{Break Points Saved}{percentage of break points faced that player won}
\item{Service Games Played}{number of service games played}
\item{Service Games Won}{percentage of service games played that were won}
\item{Service Points Won}{percentage of service points won}
\item{1st Serve Return Points Won}{percentage of first serve return points won}
\item{2nd Serve Return Points Won}{percentage of second serve return points won}
\item{Break Points Opportunities}{number of break point opportunities}
\item{Break Points Converted}{percentage of break point opportunities won}
\item{Return Games Played}{number of return games played} 
\item{Return Games Won}{percentage of return games won}
\item{Return Points Won}{percentage of return points won}
\item{Total Points Won}{percentage of service and return points won}
\item{player}{player full name}
}
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_summary(c("Nadal","Ferrer"), year = 2012, surface = "Clay")

}