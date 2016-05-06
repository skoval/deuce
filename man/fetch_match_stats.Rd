\name{fetch_match_stats}
\alias{fetch_match_stats}

\title{
Download ATP player's singles match statistics for a specified year
}
\description{
Creates a data frame with the opponent and match statistics (where available) in a given year
}

\usage{
fetch_match_stats(player, year)
}

\arguments{
  \item{player}{character value of player name}
  \item{year}{numeric year of tournament play}
}


\value{

Data frame with the columns:

\describe{
    \item{Time }{ Duration of match in HH:MM:SS if available},
    \item{Player1}{Character name of player in 1 position},
    \item{Player2}{Character name of player in 2 position},
    \item{Aces.1}{Numeric of value for Player 1},
		\item{AcesPercentage.1}{Numeric of value for Player 1},
		\item{DoubleFaults.1}{Numeric of value for Player 1},
		\item{DoubleFaultsPercentage.1}{Numeric of value for Player 1},
		\item{FirstServePercentage.1}{Numeric of value for Player 1},
		\item{FirstServeDividend.1}{Numeric of value for Player 1},
		\item{FirstServeDivisor.1}{Numeric of value for Player 1},
		\item{FirstServePointsWonPercentage.1}{Numeric of value for Player 1},
		\item{FirstServePointsWonDividend.1}{Numeric of value for Player 1},
		\item{FirstServePointsWonDivisor.1}{Numeric of value for Player 1},
		\item{SecondServePointsWonPercentage.1}{Numeric of value for Player 1},
		\item{SecondServePointsWonDividend.1}{Numeric of value for Player 1},
		\item{SecondServePointsWonDivisor.1}{Numeric of value for Player 1},
		\item{BreakPointsSavedPercentage.1}{Numeric of value for Player 1},
		\item{BreakPointsSavedDividend.1}{Numeric of value for Player 1},
		\item{BreakPointsSavedDivisor.1}{Numeric of value for Player 1},
		\item{ServiceGamesPlayed.1}{Numeric of value for Player 1},
		\item{ServiceGamesPlayedPercentage.1}{Numeric of value for Player 1},
		\item{FirstServeReturnPointsPercentage.1}{Numeric of value for Player 1},
		\item{FirstServeReturnPointsDividend.1}{Numeric of value for Player 1},
		\item{FirstServeReturnPointsDivisor.1}{Numeric of value for Player 1},
		\item{SecondServePointsPercentage.1}{Numeric of value for Player 1},
		\item{SecondServePointsDividend.1}{Numeric of value for Player 1},
		\item{SecondServePointsDivisor.1}{Numeric of value for Player 1},
		\item{BreakPointsConvertedPercentage.1}{Numeric of value for Player 1},
		\item{BreakPointsConvertedDividend.1}{Numeric of value for Player 1},
		\item{BreakPointsConvertedDivisor.1}{Numeric of value for Player 1},
		\item{ReturnGamesPlayed.1}{Numeric of value for Player 1},
		\item{ReturnGamesPlayedPercentage.1}{Numeric of value for Player 1},
		\item{TotalServicePointsWonPercentage.1}{Numeric of value for Player 1},
		\item{TotalServicePointsWonDividend.1}{Numeric of value for Player 1},
		\item{TotalServicePointsWonDivisor.1}{Numeric of value for Player 1},
		\item{TotalReturnPointsWonPercentage.1}{Numeric of value for Player 1},
		\item{TotalReturnPointsWonDividend.1}{Numeric of value for Player 1},
		\item{TotalReturnPointsWonDivisor.1}{Numeric of value for Player 1},
		\item{TotalPointsWonPercentage.1}{Numeric of value for Player 1},
		\item{TotalPointsWonDividend.1}{Numeric of value for Player 1},
		\item{TotalPointsWonDivisor.1}{Numeric of value for Player 1},
		\item{Aces.2}{Numeric of value for Player 2},
		\item{AcesPercentage.2}{Numeric of value for Player 2},
		\item{DoubleFaults.2}{Numeric of value for Player 2},
		\item{DoubleFaultsPercentage.2}{Numeric of value for Player 2},
		\item{FirstServePercentage.2}{Numeric of value for Player 2},
		\item{FirstServeDividend.2}{Numeric of value for Player 2},
		\item{FirstServeDivisor.2}{Numeric of value for Player 2},
		\item{FirstServePointsWonPercentage.2}{Numeric of value for Player 2},
		\item{FirstServePointsWonDividend.2}{Numeric of value for Player 2},
		\item{FirstServePointsWonDivisor.2}{Numeric of value for Player 2},
		\item{SecondServePointsWonPercentage.2}{Numeric of value for Player 2},
		\item{SecondServePointsWonDividend.2}{Numeric of value for Player 2},
		\item{SecondServePointsWonDivisor.2}{Numeric of value for Player 2},
		\item{BreakPointsSavedPercentage.2}{Numeric of value for Player 2},
		\item{BreakPointsSavedDividend.2}{Numeric of value for Player 2},
		\item{BreakPointsSavedDivisor.2}{Numeric of value for Player 2},
		\item{ServiceGamesPlayed.2}{Numeric of value for Player 2},
		\item{ServiceGamesPlayedPercentage.2}{Numeric of value for Player 2},
		\item{FirstServeReturnPointsPercentage.2}{Numeric of value for Player 2},
		\item{FirstServeReturnPointsDividend.2}{Numeric of value for Player 2},
		\item{FirstServeReturnPointsDivisor.2}{Numeric of value for Player 2},
		\item{SecondServePointsPercentage.2}{Numeric of value for Player 2},
		\item{SecondServePointsDividend.2}{Numeric of value for Player 2},
		\item{SecondServePointsDivisor.2}{Numeric of value for Player 2},
		\item{BreakPointsConvertedPercentage.2}{Numeric of value for Player 2},
		\item{BreakPointsConvertedDividend.2}{Numeric of value for Player 2},
		\item{BreakPointsConvertedDivisor.2}{Numeric of value for Player 2},
		\item{ReturnGamesPlayed.2}{Numeric of value for Player 2},
		\item{ReturnGamesPlayedPercentage.2}{Numeric of value for Player 2},
		\item{TotalServicePointsWonPercentage.2}{Numeric of value for Player 2},
		\item{TotalServicePointsWonDividend.2}{Numeric of value for Player 2},
		\item{TotalServicePointsWonDivisor.2}{Numeric of value for Player 2},
		\item{TotalReturnPointsWonPercentage.2}{Numeric of value for Player 2},
		\item{TotalReturnPointsWonDividend.2}{Numeric of value for Player 2},
		\item{TotalReturnPointsWonDivisor.2}{Numeric of value for Player 2},
		\item{TotalPointsWonPercentage.2}{Numeric of value for Player 2},
		\item{TotalPointsWonDividend.2}{Numeric of value for Player 2},
		\item{TotalPointsWonDivisor.2}{Numeric of value for Player 2}		

 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_match_stats("Roger Federer", 2012)

}