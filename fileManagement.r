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
	
	# Creates a folder, and all the required upstream folders if they don't exist
	
	parent <- getParentPath(path)
	
	if(!dir.exists(parent)) {
		mkdirRecursive(parent)
	}
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
