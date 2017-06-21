joinPath <- function(..., sep = .Platform$file.sep){
	
	# Makes a conventional path; it corrects the error in R that if one element
	# of the path ends in "/" it adds yet one extra "/" resulting in "//"
	
	# ... - character vectors each of length 1
	# sep - separator character
	
	# returns - character vector of length 1
	
	paths <- list(...)
	for(key in paths)
		if(!is.character(key) | length(key) > 1) stop("Elements have to character vectors of length 1")
	
	paths <- lapply(paths, function(x) gsub(paste0(sep, "$"), "", x))
	
	return( do.call(file.path, paths) )

}

mkdirRecursive <- function(path) {
	
	# Creates a folder, and all the required upstream folder if they don't exits
	
	parent <- getParentPath(path)
	
	if(!dir.exists(parent)) {
		mkdirRecursive(parent)
	} #else {
		dir.create(path)
	#}
}

getParentPath <- function(path, sep = .Platform$file.sep) {
	
	# Gets the path rigth above the current path
	# 
	# path - character vector of length one
	#
	# returns - character vector of length one
	
	if(!is.character(path) | length(path) > 1) stop("Only one character path is accepted")
	
	elements <- unlist(strsplit(path, paste0("(?<!\\\\)", sep), perl = T))
	parent <- paste0(elements[-length(elements)], collapse = sep)
	
	return(parent)
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
	
	
