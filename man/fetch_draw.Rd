\name{fetch_draw}
\alias{fetch_draw}

\title{
Download ATP tournament results.
}
\description{
Creates a data frame of draw and results of ATP tournaments of the 250 level and higher.
}

\usage{
fetch_draw(tournament, year)
}

\arguments{
  \item{tournament}{character of tournament name keyword}
  \item{year}{numeric year}
}

\details{
Data frame will include the best result for each player.
}

\value{

Data frame with the columns:

\describe{
\item{Player}{factor of player names}                     
\item{Round}{numeric of furthest round reached}                   
\item{Points}{numeric ATP points earned}                       
\item{surface}{factor of tournament surface}
\item{year}{numeric}              
\item{Seed}{character of tournament seeding}
\item{Order}{numeric order of matches}                 
\item{Date}{character of date of tournament start}      
\item{Title}{character name of tournament}
  }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{
fetch_draw(tournament = "Canada", year = 2013)
}