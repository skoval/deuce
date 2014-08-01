\name{fetch_demographics}
\alias{fetch_demographics}

\title{
Download ATP player demographics
}
\description{
Creates a data frame of demographic characteristics for a specified set of player based on the current data provided on the ATP tour website.
}
\usage{
fetch_demographics(players)
}

\arguments{
  \item{players}{vector of player names (partial matching is attempted)}
}


\value{

Data frame with the columns:

\describe{
\item{Age}{numeric current age}
\item{Birthdate}{character representation of birthdate, dd.mm.yyyy}
\item{Birthplace}{character city, country of place of birth}
\item{Residence}{character city, country of current residence}
\item{Height}{numeric height in cm}
\item{Weight}{numeric weight in kg}
\item{Handedness}{character name of dominant playing hand}
\item{TurnedPro}{numeric calendar year became ATP professional}
\item{Rank}{numeric highest ATP singles ranking}
\item{Player}{character vector of player names}
 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_demographics(c("Nadal","Ferrer"))

}