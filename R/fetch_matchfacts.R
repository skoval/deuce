fetch_matchfacts_url <- function(tournament="ATP World Tour Masters 1000 Madrid", year=2012, round="R64", player="H940"){
	
	       tournaments <- structure(list(code = c(807L, 6116L, 421L, 422L, 404L, 1536L, 
403L, 410L, 352L, 416L, 5014L, 301L, 580L, 1720L, 425L, 328L, 
316L, 747L, 339L, 773L, 506L, 360L, 891L, 499L, 451L, 495L, 741L, 
468L, 314L, 500L, 414L, 717L, 319L, 6003L, 311L, 496L, 402L, 
341L, 375L, 438L, 308L, 315L, 6120L, 6932L, 520L, 407L, 424L, 
533L, 440L, 568L, 429L, 321L, 338L, 329L, 439L, 560L, 573L, 337L, 
505L, 418L, 540L, 6242L, 2276L), location = structure(c(1L, 2L, 
3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 
30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 43L, 
44L, 46L, 47L, 48L, 50L, 51L, 49L, 53L, 54L, 55L, 56L, 57L, 58L, 
42L, 59L, 60L, 61L, 62L, 52L, 45L, 63L), .Label = c("Acapulco", 
"Atlanta", "ATP World Tour Masters 1000 Canada", "ATP World Tour Masters 1000 Cincinnati", 
"ATP World Tour Masters 1000 Indian Wells", "ATP World Tour Masters 1000 Madrid", 
"ATP World Tour Masters 1000 Miami", "ATP World Tour Masters 1000 Monte Carlo", 
"ATP World Tour Masters 1000 Paris", "ATP World Tour Masters 1000 Rome", 
"ATP World Tour Masters 1000 Shanghai", "Auckland", "Australia", 
"Bangkok", "Barcelona", "Basel", "Bastad", "Beijing", "Brisbane", 
"Bucharest", "Buenos Aires", "Casablanca", "Chennai", "Delray Beach", 
"Doha", "Dubai", "Eastbourne", "Estoril", "Gstaad", "Halle", 
"Hamburg", "Houston", "Kitzbuhel", "Kuala Lumpur", "London / Queen's Club", 
"Marseille", "Memphis", "Metz", "Montpellier", "Moscow", "Munich", 
"New York", "Newport", "Nice", "North Carolina", "Rio de Janeiro", 
"Roland Garros", "Rotterdam", "s-Hertogenbosch", "San Jose", 
"Sao Paulo", "Southfields", "St. Petersburg", "Stockholm", "Stuttgart", 
"Sydney", "Tokyo", "Umag", "Valencia", "Vienna", "Vina del Mar", 
"Washington", "Zagreb"), class = "factor"), tournament = structure(c(1L, 
9L, 48L, 61L, 14L, 38L, 53L, 36L, 13L, 32L, 51L, 30L, 7L, 55L, 
8L, 54L, 52L, 18L, 17L, 16L, 20L, 28L, 5L, 22L, 45L, 23L, 4L, 
44L, 21L, 27L, 11L, 25L, 10L, 34L, 3L, 40L, 57L, 37L, 42L, 33L, 
12L, 29L, 41L, 47L, 26L, 2L, 50L, 15L, 56L, 39L, 31L, 35L, 6L, 
46L, 60L, 58L, 59L, 24L, 49L, 19L, 62L, 63L, 43L), .Label = c("Abierto Mexicano Telcel", 
"ABN AMRO World Tennis Tournament", "Aegon Championships", "Aegon International", 
"Aircel Chennai Open", "Apia International Sydney", "Australian Open", 
"Barcelona Open Banc Sabadell", "BB&T Atlanta Open", "bet-at-home Cup Kitzbuhel", 
"bet-at-home Open", "BMW Open by FWU AG", "BNP Paribas Masters", 
"BNP Paribas Open", "Brasil Open 2014", "BRD Nastase Tiriac Trophy", 
"Brisbane International presented by Suncorp", "China Open", 
"Citi Open", "Copa Claro", "Credit Agricole Suisse Open Gstaad", 
"Delray Beach Open by The Venetian Las Vegas", "Dubai Duty Free Tennis Championships", 
"Erste Bank Open", "Fayez Sarofim & Co. U.S. Men's Clay Court Championship", 
"French Open", "Gerry Weber Open", "Grand Prix Hassan II", "Hall of Fame Tennis Championships", 
"Heineken Open", "If Stockholm Open", "Internazionali BNL d'Italia", 
"Kremlin Cup by Bank of Moscow", "Malaysian Open, Kuala Lumpur", 
"MercedesCup", "Monte-Carlo Rolex Masters", "Moselle Open", "Mutua Madrid Open", 
"Negev Israel Open", "Open 13", "Open de Nice Cote d'Azur", 
"Open Sud de France", "PBZ Zagreb Indoors", "Portugal Open", 
"Qatar ExxonMobil Open", "Rakuten Japan Open Tennis Championships", 
"Rio Open presented by Claro hdtv", "Rogers Cup", "Royal Guard Open Chile", 
"SAP Open", "Shanghai Rolex Masters", "SkiStar Swedish Open", 
"Sony Open Tennis", "Swiss Indoors Basel", "Thailand Open", "Topshelf Open", 
"U.S. National Indoor Tennis Championships", "US Open", "Valencia Open 500", 
"Vegeta Croatia Open Umag", "Western & Southern Open - Cincinnati", 
"Wimbledon", "Winston-Salem Open"), class = "factor"), tier = structure(c(2L, 
1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 3L, 1L, 2L, 2L, 1L, 
2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 3L, 2L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 2L, 1L, 3L, 1L, 2L, 1L, 2L, 3L, 1L, 1L), .Label = c("250", 
"500", "Grand Slam", "Masters 1000"), class = "factor"), rounds = c(5L, 
5L, 6L, 6L, 7L, 6L, 7L, 6L, 6L, 6L, 6L, 5L, 7L, 5L, 6L, 5L, 5L, 
5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 5L, 5L, 
5L, 6L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 7L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 5L, 5L, 5L, 7L, 5L, 5L, 5L, 6L, 7L, 6L, 5L), surface = structure(c(3L, 
1L, 3L, 3L, 3L, 1L, 3L, 1L, 3L, 1L, 3L, 3L, 3L, 3L, 1L, 3L, 1L, 
3L, 3L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
3L, 2L, 3L, 3L, 3L, 3L, 3L, 1L, 2L, 1L, 1L, 1L, 3L, 3L, 1L, 2L, 
3L, 3L, 1L, 3L, 3L, 1L, 3L, 3L, 3L, 1L, 3L, 2L, 3L, 3L), .Label = c("Clay", 
"Grass", "Hard"), class = "factor")), .Names = c("code", "location", 
"tournament", "tier", "rounds", "surface"), class = "data.frame", row.names = c(NA, 
-63L))

 	check.tournament <- grep(tournament, tournaments$location)
    if(length(check.tournament)==0) check.tournament <- grep(tournament, tournaments$tournament)
    if(length(check.tournament)==0)
    	stop("Tournament not found.")
    
    if(length(check.tournament)>1){
    	check.tournament <- check.tournament[1]
    	warning("Multiple tournament matches. Taking first.")
    }
	
	tournament <- tournaments$code[check.tournament]    

	rounds <- c("R128","R64","R32","R16","Q","S","F")
	round <- which(rounds==round)
	
	base.url <- "http://www.atpworldtour.com/Share/Match-Facts-Pop-Up.aspx?"
	
	match.characteristic <- paste(paste("t=",tournament,collapse="",sep=""),
								  paste("y=",year,collapse="",sep=""),
								  paste("r=",round,collapse="",sep=""),
								  paste("p=",player,collapse="",sep=""),sep="&")
	
	file <- paste(base.url,match.characteristic,collapse="",sep="")

file
}


Tags <- c("Winner","Player","Opponent","Time","Aces",
			"Double Faults",
			"1st Serve In","1st Serves",
			"1st Serve Points Won",
			"2nd Serve Points Won","2nd Serve In",
			"Break Points Saved","Break Points Faced",
			"Service Games Played",
			"1st Serve Return Points Won","1st Serve Return Points",
			"2nd Serve Return Points Won","2nd Serve Return Points",
			"Break Points Converted","Break Points Opportunities",
			"Return Games Played",
			"Total Service Points Won","Total Service Points",
			"Total Return Points Won","Total Return Points",
			"Total Points Won","Total Points")

is_match <- function(lines){
	
	NotMatch <- length(grep("(/)",lines,fixed=TRUE))>0
	
	if(!NotMatch){
		NoData <- length(grep("N/A Bye",lines,fixed=TRUE))>0
		}
	else{
		NoData <- TRUE
	}

!NoData
}			

check_winner <- function(winner.name, code){
	if(length(grep(code,players$code))>0){ # IF PLAYER CODE IN 
		player.name <- players$player[which(players$code==code)]
		return(as.numeric(player.name==winner.name))
	}
	else{
		return(NA)
	}
}


matchfacts_values <- function(lines, code) {
    
    if (is_match(lines)) {
        
        # Player Names
        player.name.index <- grep("playerName", lines)
        player.name <- sub("(.*playerName.>)([a-zA-Z].*)(<\\/a.*)", "\\2", lines[player.name.index])
        
        # Get minutes played
        time.index <- grep("minutes", lines)
        time <- sub("(.*>)([0-9]+)(&nbsp.*)", "\\2", lines[time.index])
        
        # firstCells with stat
        index <- grep("firstCell.*>[0-9].*", lines)
        
        # fraction fields
        fraction.index <- grep("[0-9]\\/", lines[index])
        not.fraction.index <- (1:length(index))[is.na(match(1:length(index), fraction.index))]
        
        replacements <- sub("(.*>)([0-9]+)(<.*)", "\\2", lines[index[not.fraction.index]])
        fraction.replacements <- sub("(.*\\()([0-9]+\\/[0-9]+)(\\).*)", "\\2", lines[index[fraction.index]])
        
        index[not.fraction.index] <- replacements
        index[fraction.index] <- fraction.replacements
        
        fields <- lapply(index, function(x) strsplit(x, "/")[[1]])
        fields <- as.numeric(unlist(fields))
        fields <- c(check_winner(player.name[1], code), player.name[2:3], time, fields)

        # REMOVE EXTRA FIRST SERVE IN
        fields <- fields[-10]
    } else {
        fields <- rep(NA, length(Tags))
    }
    
    result <- data.frame(
    	V1 = as.numeric(fields[1]),
    	V2 = as.character(fields[2]),
    	V3 = as.character(fields[3]),
    	V4 = as.numeric(fields[4]),
    	V5 = as.numeric(fields[5]),
    	V6 = as.numeric(fields[6]),
    	V7 = as.numeric(fields[7]),
    	V8 = as.numeric(fields[8]),
    	V9 = as.numeric(fields[9]),
    	V10 = as.numeric(fields[10]),
    	V11 = as.numeric(fields[11]),
    	V12 = as.numeric(fields[12]),
    	V13 = as.numeric(fields[13]),
    	V14 = as.numeric(fields[14]),
    	V15 = as.numeric(fields[15]),
    	V16 = as.numeric(fields[16]),
    	V17 = as.numeric(fields[17]),
    	V18 = as.numeric(fields[18]),
    	V19 = as.numeric(fields[19]),
    	V20 = as.numeric(fields[20]),
    	V21 = as.numeric(fields[21]),
    	V22 = as.numeric(fields[22]),
    	V23 = as.numeric(fields[23]),
    	V24 = as.numeric(fields[24]),
    	V25 = as.numeric(fields[25]),
    	V26 = as.numeric(fields[26]),
    	V27 = as.numeric(fields[27]),
    	stringsAsFactors = FALSE
    	)
    	
     names(result) <- Tags

result     
}

fetch_matchfacts_apply <- function(tournament = "Madrid", year = 2012, round = "R64", player = "Nadal") {
    
    URLMatchFacts <- fetch_matchfacts_url(tournament, year, round, player)
    URLMatchFacts <- url(URLMatchFacts)
    MatchContent <- readLines(con = URLMatchFacts, warn = FALSE)
    close(URLMatchFacts)
    
matchfacts_values(MatchContent, player)
}

fetch_matchfacts <- function(tournament = "Madrid", year = 2012, player = "Nadal") {
    
    players <- structure(list(player = c("Novak Djokovic", "Rafael Nadal", "Roger Federer", 
"Andy Murray", "Jo-Wilfried Tsonga", "David Ferrer", "Tomas Berdych", 
"Janko Tipsarevic", "Mardy Fish", "John Isner", "Gilles Simon", 
"Juan Martin Del Potro", "Nicolas Almagro", "Gael Monfils", "Juan Monaco", 
"Feliciano Lopez", "Kei Nishikori", "Richard Gasquet", "Fernando Verdasco", 
"Alexandr Dolgopolov", "Florian Mayer", "Stanislas Wawrinka", 
"Milos Raonic", "Marin Cilic", "Radek Stepanek", "Julien Benneteau", 
"Marcel Granollers", "Andy Roddick", "Viktor Troicki", "Juan Ignacio Chela", 
"Kevin Anderson", "Robin Soderling", "Bernard Tomic", "Philipp Kohlschreiber", 
"Jurgen Melzer", "Mikhail Youzhny", "Carlos Berlocq", "Pablo Andujar", 
"Marcos Baghdatis", "Thomaz Bellucci", "Alex Bogomolov Jr", "Albert Ramos", 
"Robin Haase", "Jarkko Nieminen", "Denis Istomin", "Andreas Seppi", 
"Michael Llodra", "Fabio Fognini", "David Nalbandian", "Lukasz Kubot", 
"Donald Young", "Juan Carlos Ferrero", "Santiago Giraldo", "Mikhail Kukushkin", 
"Alejandro Falla", "Nikolay Davydenko", "Yen-Hsun Lu", "Ryan Harrison", 
"Go Soeda", "Jeremy Chardy", "Ivo Karlovic", "Gilles Muller", 
"Dudi Sela", "Xavier Malisse", "Lukas Lacko", "Olivier Rochus", 
"Steve Darcis", "Sergiy Stakhovsky", "Albert Montanes", "Flavio Cipolla", 
"Leonardo Mayer", "Matthew Ebden", "Filippo Volandri", "Ivan Dodig", 
"Guillermo Garcia-Lopez", "Sam Querrey", "Potito Starace", "Igor Andreev", 
"Lukas Rosol", "Tatsuma Ito", "Tobias Kamke", "Paolo Lorenzi", 
"Edouard Roger-Vasselin", "Ernests Gulbis", "Malek Jaziri", "Dmitry Tursunov", 
"Bjorn Phau", "Igor Kunitsyn", "Nicolas Mahut", "Rui Machado", 
"Cedrik-Marcel Stebe", "James Blake", "Philipp Petzschner", "Grigor Dimitrov", 
"Adrian Ungur", "Benoit Paire", "Ruben Ramirez Hidalgo", "Simone Bolelli", 
"Vasek Pospisil", "Pere Riba", "Martin Klizan", "Blaz Kavcic", 
"Matthias Bachinger", "Victor Hanescu", "Daniel Brands", "Michael Russell", 
"Daniel Gimeno-Traver", "David Goffin", "Frederico Gil", "Andreas Beck", 
"Karol Beck", "Michael Berrer", "Wayne Odesnik", "Paul Capdeville", 
"Eric Prodon", "Joao Souza", "Frank Dancevic", "Horacio Zeballos", 
"Adrian Mannarino", "Jurgen Zopp", "Rogerio Dutra Silva", "Marinko Matosevic", 
"Igor Sijsling", "Rajeev Ram", "Federico Delbonis", "Alessandro Giannessi", 
"Antonio Veic", "Jesse Levine", "Roberto Bautista-Agut", "Marsel Ilhan", 
"Ryan Sweeting", "Grega Zemlja", "Bobby Reynolds", "Tommy Haas", 
"Marco Chiudinelli", "Jan Hajek", "James Ward", "Arnaud Clement", 
"Rik De Voest", "Joao Sousa", "Ricardo Mello", "Andrey Golubev", 
"Augustin Gensse", "Diego Junqueira", "Ruben Bemelmans", "Evgeny Donskoy", 
"Julio Silva", "Daniel Munoz-De La Nava", "Marc Gicquel", "Martin Alund", 
"Florent Serra", "Andreas Haider-Maurer", "Thiago Alves", "Guillaume Rufin", 
"Benjamin Becker", "Aljaz Bedene", "Andrey Kuznetsov", "Izak Van Der Merwe", 
"Dustin Brown", "Stephane Robert", "Matteo Viola", "Pablo Carreno-Busta", 
"Thomas Schoorel", "Alexander Kudryavtsev", "Teymuraz Gabashvili", 
"Kenny De Schepper", "Yuichi Sugita", "James Duckworth", "Rainer Schuettler", 
"Sergei Bubka", "Denis Kudla", "Michael Yani", "Danai Udomchoke", 
"Javier Marti", "Tsung-Hua Yang", "Lleyton Hewitt", "Ricardas Berankis", 
"Maxime Authom", "Eduardo Schwank", "Carlos Salamanca", "Ivo Minar", 
"Jonathan Dasnieres De Veigy", "Arnau Brugues-Davi", "Robby Ginepri", 
"Jan Mertl", "Amir Weintraub", "Jan Hernych", "Inigo Cervantes", 
"Maximo Gonzalez", "Konstantin Kravchuk", "Dusan Lajovic", "Jimmy Wang", 
"Laurynas Grigelis", "Gregoire Burquier", "Facundo Bagnis", "Gastao Elias", 
"Nikola Ciric", "Peter Gojowczyk", "Mischa Zverev", "Peter Torebko"
), code = c("D643", "N409", "F324", "MC10", "T786", "F401", "BA47", 
"T742", "F339", "I186", "SD32", "D683", "A479", "MC65", "MA21", 
"L397", "N552", "G628", "V306", "D801", "MB02", "W367", "R975", 
"C977", "S694", "B747", "G710", "R485", "T840", "C514", "A678", 
"SA49", "TA46", "K435", "M762", "Y061", "B884", "A596", "B837", 
"BD20", "B842", "R772", "H756", "N289", "I165", "SA93", "L428", 
"F510", "N301", "K540", "Y124", "F316", "G725", "K926", "F444", 
"D402", "L575", "H940", "SC47", "CA12", "K336 ", "MA30", "SC56", 
"M680", "L797", "R397", "D632", "SC77", "M824", "C723", "MD56", 
"E690", "V254", "D646", "G476", "Q927", "S843", "A511", "R685", 
"I230", "K839", "L503", "R613", "G858", "J267", "T315", "P436", 
"K403", "M873", "MA49", "SK94", "B676", "P624", "D875", "U061", 
"PD31", "R383", "BA98", "PD07", "R750", "K966", "K834", "BC65", 
"H528", "BC72", "R368", "G676", "GB88", "G717", "BA86", "B804", 
"B678", "O234", "C634", "P487", "SG64", "D499", "Z184", "ME82", 
"Z254", "SC73", "ME05", "SF36", "R548", "D874", "G983", "V564", 
"L799", "BD06", "K771", "SG87", "Z189", "R483", "H355", "C582", 
"H571", "W503", "C487", "D424", "SH90", "M708", "G806", "G695", 
"J214", "BE54", "D864", "S877", "M915", "G436", "A539", "S963", 
"H804", "A410", "RA17", "B896", "BH09", "KB54", "V379", "BA65", 
"R612", "V490", "", "CD85", "K596", "G681", "D750", "SE73", "D994", 
"S636", "BC11", "KB09", "Y070", "U043", "M108", "T982", "H432", 
"BE90", "A789", "SE66", "SB28", "MA41", "D729", "BA77", "G569", 
"MA38", "W471", "H442", "CA33", "G585", "K776", "L987", "W373", 
"GA71", "BC36", "BF23", "E698", "C706", "G976", "Z168", "T911"
)), .Names = c("player", "code"), class = "data.frame", row.names = c(NA, 
-200L))
    
    # EXPAND (tournament, year) FOR EACH ROUND AND PLAYER
    rounds <- c("R128", "R64", "R32", "R16", "Q", "S", "F")
    is.player <- grep(player, players$player)
    if(length(is.player) == 0) stop("Player not found.")
    player <- players$code[is.player[1]]
    Round <- rep(rounds, length(tournament) * length(player))
    Player <- rep(rep(player, each = length(rounds)), length(tournament))
    Year <- rep(year, length = length(Player))
    Tournament <- rep(tournament, each = length(player) * length(rounds))
    
    Result <- do.call("rbind",
    			mapply(fetch_matchfacts_apply, tournament = Tournament, year = Year, round = Round, player = player, SIMPLIFY = FALSE))
    
    NoMatch <- apply(Result, 1, function(x) all(is.na(x)))
    Result <- Result[!NoMatch,]
    row.names(Result) <- 1:nrow(Result)
    
    Result$Tournament <- Tournament[!NoMatch]
    Result$Year <- Year[!NoMatch]
    Result$Round <- Round[!NoMatch]
    
Result
} 
