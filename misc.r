printTime <- function(x = "") {
	# Prints to console the current date and time followed by
	# the message(s) in x
	flush.console()
	cat(as.character(Sys.time()), x)
}

trim <- function(x) { 
		
	# Trims white space from a character vector
	
	if(!is.character(x))
		stop("x has to be string or a character vector")

	return(gsub("\\s+", "", x))
	
}

parseArg <- function(x, sep, trim = T) {
		
	#Takes a string an creates a vector using sep as the separator
	# x - a string, if a vector only first element will be considred
	# sep - a string, if a vector only first element will be considered
	# trim - bool, if true white spaces will be eliminated
		
	if(!is.character(x))
		stop ("x has to be a string")
	if(!is.character(sep))
		stop ("x has to be a string")
	
	x <- x[1]
	sep <- sep[1]
	
	if(trim)
		x <- trim(x)
	
	return (unlist(strsplit(x,sep)))
	
}

