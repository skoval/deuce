fetch_url <- function(name) {
    
    url <- "http://www.atpworldtour.com/Search/Players.aspx?q=name&f=&or="
    url <- "http://www.atpworldtour.com/Search/Players.aspx?q=name&f=&or="
    
    result <- readLines(sub("name", name, url), warn = FALSE)
    result <- result[grep("/Tennis/Players/", result)]  # Subset to player pages
    
    # Split
    query.terms <- strsplit(name, split = " ")[[1]]
    indices <- unlist(sapply(query.terms, function(x) grep(x, result)))
    indices <- table(indices)
    
    if (any(indices == length(query.terms))) {
        index <- as.numeric((names(indices)[indices == length(query.terms)])[1])
        sub("(.*)(Tennis/Players/.*aspx)(.*)", "\\2", result[index])
    } else if (length(query.terms)>1){
   
    	url <- "/Tennis/Players/Last/First/Full-Name.aspx"
    	F <- substr(query.terms[1],1,1)
    	L <- substr(query.terms[length(query.terms)],1,2)
    	url <- sub("First", F, url)
    	url <- sub("Last", L, url)
    	url <- sub("Full-Name",paste(query.terms,sep="",collapse="-"),url)
    	test.url <- tryCatch(open(url(paste("http://www.atpworldtour.com",url,sep=""))), error = function(x) NA)
    	if(is.na(test.url)){
    		url <- "/Tennis/Players/Top-Players/Full-Name.aspx"	
    		url <- sub("Full-Name",paste(query.terms,sep="",collapse="-"),url)
    	}    		
      url
    }
    else{
    	NA
    }
} 
