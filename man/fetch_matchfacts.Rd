\name{fetch_matchfacts}
\alias{fetch_matchfacts}

\title{
Download ATP player match-specific statistics
}
\description{
Creates a data frame of service and return statistics for all matches for specified player, tournament, and year.
}

\usage{
fetch_matchfacts(tournament = "Madrid", year = 2012, player = "Nadal")
}

\arguments{
  \item{tournament}{character of tournament name keyword}
  \item{year}{numeric year}
  \item{player}{character name of player (partial matching is attempted)}
}

\details{
Data frame will include all match results for each player and tournament.
}

\value{

Data frame with the columns:

\describe{
\item{Winner}{numeric indicator (1=win, 0=loss)}                     
\item{Player}{character}                     
\item{Opponent}{character}                   
\item{Time}{numeric duration of match in minutes}                       
\item{Aces}{numeric}                       
\item{Double Faults}{numeric}              
\item{1st Serve In}{numeric}               
\item{1st Serves}{numeric}                 
\item{1st Serve Points Won}{numeric}      
\item{2nd Serve Points Won}{numeric}       
\item{2nd Serve In}{numeric}               
\item{Break Points Saved}{numeric}         
\item{Break Points Faced}{numeric}         
\item{Service Games Played}{numeric}       
\item{1st Serve Return Points Won}{numeric}
\item{1st Serve Return Points}{numeric}    
\item{2nd Serve Return Points Won}{numeric}
\item{2nd Serve Return Points}{numeric}    
\item{Break Points Converted}{numeric}     
\item{Break Points Opportunities}{numeric} 
\item{Return Games Played}{numeric}        
\item{Total Service Points Won}{numeric}   
\item{Total Service Points}{numeric}       
\item{Total Return Points Won}{numeric}    
\item{Total Return Points}{numeric}        
\item{Total Points Won}{numeric}           
\item{Total Points}{numeric}
\item{Tournament}{character tournament keyword}
\item{Year}{numeric}
\item{Round}{character}
}
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

data(players)
data(tournaments)
fetch_matchfacts(tournament = "Canada", year = 2013, player = "Raonic")
}