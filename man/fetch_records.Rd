\name{fetch_records}
\alias{fetch_records}

\title{
Download ATP player match record statistics
}
\description{
Creates a data frame of 72 match record statistics for year-to-date (YTD) and career.
}

\usage{
fetch_records(players)
}

\arguments{
  \item{players}{vector of player names (partial matching is attempted)}
}


\value{

Data frame with the columns:

\describe{
  \item{Overall YTD Won}{year-to-date matches won}               
  \item{Grand Slams YTD Won}{year-to-date matches in grand slams won}           
  \item{Masters 1000 YTD Won}{year-to-date matches in masters 1000 won}          
  \item{Tiebreaks YTD Won}{year-to-date sets won in a tiebreak}             
  \item{Versus Top 10 YTD Won}{year-to-date sets won against top 10 ranked player}         
  \item{Finals YTD Won}{year-to-date titles won}
  \item{Deciding Set YTD Won}{year-to-date matches won}          
  \item{5th Set YTD Won}{year-to-date matches won in a 5th set}               
  \item{Hard YTD Won}{year-to-date matches won on hard court}                   
 \item{Clay YTD Won}{year-to-date matches won on clay court}                   
 \item{Grass YTD Won}{year-to-date matches won on grass court}                  
 \item{Carpet YTD Won}{year-to-date matches won on carpet court}                 
 \item{Indoor YTD Won}{year-to-date matches won on indoor court}                 
 \item{Outdoor YTD Won}{year-to-date matches won on outdoor court}                
 \item{After 1st Set Win YTD Won}{year-to-date matches won after 1st set win}      
 \item{After 1st Set Loss YTD Won}{year-to-date matches won after 1st set loss}     
 \item{vs. Righties YTD Won}{year-to-date matches won against a right-handed player}           
 \item{vs. Lefties YTD Won}{year-to-date matches won against a left-handed player}            
 \item{Overall YTD Lost}{year-to-date matches lost}              
 \item{Grand Slams YTD Lost}{year-to-date matches in grand slams lost}           
 \item{Masters 1000 YTD Lost}{year-to-date matches in masters 1000 lost}         
 \item{Tiebreaks YTD Lost}{year-to-date sets lost in a tiebreak}             
 \item{Versus Top 10 YTD Lost}{year-to-date sets lost against top 10 ranked player}         
 \item{Finals YTD Lost}{year-to-date titles lost}               
 \item{Deciding Set YTD Lost}{year-to-date matches lost}         
 \item{5th Set YTD Lost}{year-to-date matches lost in a 5th set}             
 \item{Hard YTD Lost}{year-to-date matches lost on hard court}                 
 \item{Clay YTD Lost}{year-to-date matches lost on clay court}                 
 \item{Grass YTD Lost}{year-to-date matches lost on grass court}                 
 \item{Carpet YTD Lost}{year-to-date matches lost on carpet court}               
 \item{Indoor YTD Lost}{year-to-date matches lost on indoor court}               
 \item{Outdoor YTD Lost}{year-to-date matches lost on outdoor court}              
 \item{After 1st Set Win YTD Lost}{year-to-date matches lost after 1st set win}     
 \item{After 1st Set Loss YTD Lost}{year-to-date matches lost after 1st set loss}    
 \item{vs. Righties YTD Lost}{year-to-date matches lost against a right-handed player}         
 \item{vs. Lefties YTD Lost}{year-to-date matches lost against a left-handed player}          
  \item{Overall Career Won}{career matches won}               
  \item{Grand Slams Career Won}{career matches in grand slams won}           
  \item{Masters 1000 Career Won}{career matches in masters 1000 won}          
  \item{Tiebreaks Career Won}{career sets won in a tiebreak}             
  \item{Versus Top 10 Career Won}{career sets won against top 10 ranked player}         
  \item{Finals Career Won}{career titles won}
  \item{Deciding Set Career Won}{career matches won}          
  \item{5th Set Career Won}{career matches won in a 5th set}               
  \item{Hard Career Won}{career matches won on hard court}                   
 \item{Clay Career Won}{career matches won on clay court}                   
 \item{Grass Career Won}{career matches won on grass court}                  
 \item{Carpet Career Won}{career matches won on carpet court}                 
 \item{Indoor Career Won}{career matches won on indoor court}                 
 \item{Outdoor Career Won}{career matches won on outdoor court}                
 \item{After 1st Set Win Career Won}{career matches won after 1st set win}      
 \item{After 1st Set Loss Career Won}{career matches won after 1st set loss}     
 \item{vs. Righties Career Won}{career matches won against a right-handed player}           
 \item{vs. Lefties Career Won}{career matches won against a left-handed player}            
 \item{Overall Career Lost}{career matches lost}              
 \item{Grand Slams Career Lost}{career matches in grand slams lost}           
 \item{Masters 1000 Career Lost}{career matches in masters 1000 lost}         
 \item{Tiebreaks Career Lost}{career sets lost in a tiebreak}             
 \item{Versus Top 10 Career Lost}{career sets lost against top 10 ranked player}         
 \item{Finals Career Lost}{career titles lost}               
 \item{Deciding Set Career Lost}{career matches lost}         
 \item{5th Set Career Lost}{career matches lost in a 5th set}             
 \item{Hard Career Lost}{career matches lost on hard court}                 
 \item{Clay Career Lost}{career matches lost on clay court}                 
 \item{Grass Career Lost}{career matches lost on grass court}                 
 \item{Carpet Career Lost}{career matches lost on carpet court}               
 \item{Indoor Career Lost}{career matches lost on indoor court}               
 \item{Outdoor Career Lost}{career matches lost on outdoor court}              
 \item{After 1st Set Win Career Lost}{career matches lost after 1st set win}     
 \item{After 1st Set Loss Career Lost}{career matches lost after 1st set loss}    
 \item{vs. Righties Career Lost}{career matches lost against a right-handed player}         
 \item{vs. Lefties Career Lost}{career matches lost against a left-handed player}         
 \item{player}{player names as given in function call}  
 }
}

\references{
\url{http://www.atpworldtour.com}
}

\author{
Stephanie Kovalchik <s.a.kovalchik@gmail.com>
}


\examples{

fetch_records(c("Nadal", "Courier"))

}