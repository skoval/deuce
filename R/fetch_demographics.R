fetch_demographics_apply <- function(player) {
   
    demo_stats <- function(lines, index) {
        
        if (length(index[[2]]) == 0) {
            Birthplace <- NA
        } else {
            Birthplace <- sub("(.*span> ?)(.*)(<.li.*)", "\\2", lines[index[[2]]])
        }
        if (length(index[[3]]) == 0) {
            Res <- NA
        } else {
            Res <- sub("(.*span> ?)(.*)(<.li.*)", "\\2", lines[index[[3]]])
        }
        if (length(index[[4]]) == 0) {
            h <- NA
        } else {
            h <- sub("(.*\\()([0-9][0-9][0-9])( cm.*)", "\\2", lines[index[[4]]])
        }
        if (length(index[[5]]) == 0) {
            w <- NA
        } else {
            w <- sub("(.*\\()([0-9][0-9][0-9]?)( kg.*)", "\\2", lines[index[[5]]])
        }
        if (length(index[[6]]) == 0) {
            hand <- NA
        } else {
            hand <- sub("(.*span> ?)(.*)(<.li.*)", "\\2", lines[index[[6]]])
        }
        if (length(index[[7]]) == 0) {
            pro <- NA
        } else {
            pro <- sub("(.*span> ?)([0-9]+)(<.li.*)", "\\2", lines[index[[7]]])
        }
        
        DOB <- sub("(.*)([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9])(.*)", "\\2", lines[index[[1]]])
        Year <- sub("([0-9][0-9].)([0-9][0-9].)([0-9][0-9][0-9][0-9])", "\\3", DOB)
        
        if(!is.na(DOB)){
           CurrentDate <- as.Date(Sys.Date())
           DOBDate <- as.Date(DOB, "%d.%m.%Y")
           Age <- as.numeric(CurrentDate-DOBDate)/365.25  
        }
        else
           Age <- NA
           
        Values <- c(Age, DOB, Birthplace, Res, h, w, hand, pro)
        
        Values
    }
        
        
    call <- fetch_url(player)
    if (is.na(call)) 
        stop("Player not found.")
    
    call <- url(collapse("http://www.atpworldtour.com/", call))
    
    source <- readLines(call, ok = TRUE, warn = FALSE, n = 400, encoding = "UTF-8")
    close(call)
    
    demo_fields <- c("Age", "Birthdate", "Birthplace", "Residence", "Height", "Weight", "Plays", "Turned Pro")
    
    index <- lapply(paste(demo_fields, ":", sep = ""), function(x) grep(x, source, fixed = TRUE))
    
    if(length(index[[2]])==0)
	    Values <- demo_stats(source, index[-2])
	 else
	 	Values <- demo_stats(source, index[-1])
    
    HighRank <- sub("(.*>)([0-9]+)(<.span>.*)", "\\2", source[grep("High", source)])
    Values <- data.frame(
    	Age = as.numeric(Values[1]),
    	Birthdate = as.character(Values[2]),
    	Birthplace = as.character(Values[3]),
    	Residence = as.character(Values[4]),
    	Height = as.numeric(Values[5]),
    	Weight = as.numeric(Values[6]),
    	Plays = as.character(Values[7]),
    	TurnedPro = as.numeric(Values[8]),
    	Rank = as.numeric(HighRank[1]),
    	stringsAsFactors = FALSE
    	)

    Values
}

fetch_demographics <- function(players) {
    
    data <- do.call("rbind", lapply(players, fetch_demographics_apply))
    data$Player <- players    
data
} 
