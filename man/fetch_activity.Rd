\name{fetch_activity}
\alias{fetch_activity}

\title{
Download ATP player's singles playing activity for a specified year
}
\description{
Creates a data frame of the date, tournament played, and result for ATP singles play during a given year.
}

\usage{
fetch_activity(player, year = NULL)
}

\arguments{
  \item{player}{character value of keyword for player name}
  \item{year}{numeric year of tournament play, if \code{NULL} all years are returned}
}


\value{

Data frame with the columns:

\describe{
    \item{name }{ Character name of tournament}
        \item{location }{ Character name of city of tournament venue}
       \item{ start_date }{ Date object of start of tournament}
       \item{ end_date }{ Date object of end of tournament} 
       \item{ draw }{ Numeric size of singles draw}
        \item{matches }{  Numeric number of singles matches in tournament}
        \item{surface }{ Character description of surface}
        \item{prize }{ Character of available prize money}
        \item{score }{ Character representation of score}
        \item{round }{ Character description of round} 
        \item{winner }{ Numeric indicator if player won match}
       \item{ player }{ Character name of player}
       \item{ player_rank }{ Numeric of player ranking at start of tournament}
       \item{ opponent }{ Character name of opponent}
       \item{ opponent_rank }{ Numeric of opponent ranking at start of tournament}
       \item{ player1 }{ Numeric games won in set 1}
       \item{ player2 }{ Numeric games won in set 2}
       \item{ player3 }{ Numeric games won in set 3}
       \item{ player4 }{ Numeric games won in set 4}
       \item{ player5 }{ Numeric games won in set 5}
       \item{ opponent1 }{ Numeric games won in set 1}
       \item{ opponent2 }{Numeric games won in set 2}
       \item{ opponent3 }{ Numeric games won in set 3}
       \item{ opponent4 }{ Numeric games won in set 4}
       \item{ opponent5 }{ Numeric games won in set 5}
       \item{ TBplayer1 }{ Numeric points won in tiebreak 1}
       \item{ TBplayer2 }{ Numeric points won in tiebreak 2}
       \item{ TBplayer3 }{ Numeric points won in tiebreak 3}
       \item{ TBplayer4 }{ Numeric points won in tiebreak 4}
       \item{ TBplayer5 }{ Numeric points won in tiebreak 5}
       \item{TBopponent1 }{ Numeric points won in tiebreak 1}
       \item{TBopponent2 }{ Numeric points won in tiebreak 2}
       \item{ TBopponent3 }{ Numeric points won in tiebreak 3}
       \item{ TBopponent4 }{ Numeric points won in tiebreak 4}
       \item{ TBopponent5 }{ Numeric points won in tiebreak 5}
  
 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_activity("Roger Federer", 2012)

}