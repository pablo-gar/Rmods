printTime <- function(x = "", carriageReturn = F) {
	# Prints to console the current date and time followed by
	# the message(s) in x
	
	first = ""
	if(carriageReturn) first = "\r"
	
	flush.console()
	cat(first, as.character(Sys.time()), x)
}

#' Calcualtes bootstrap confidence interval after applying a function to a vector
#' @param x vector
#' @param FUN function to be applied to vector, has to return a numeric vector of size 1
#' @param bootstrap_counts number of bootrsap rounds
#' @param interval value between 0 and 1 depecting the confidence interval desired
#' @param bootstrap_size value between 0 and 1 indicanting the percentage of data taken at each bootstrap repetition
#' @param out_string logical, if true returns a string representation of the interval, otherwise return a vector of size 2
bootstrap_confidence_interval <- function(x, FUN, bootstrap_counts = 1000, interval = 0.95, bootstrap_size = 1, out_string = F) {
    
    bootstrap_counts <- as.integer(bootstrap_counts[1])
    interval <- interval[1]
    bootstrap_size <- bootstrap_size[1]
    out_string <- out_string[1]
    
    if(!is.numeric(x) | !is.vector(x))
        stop("x has to be a numeric vector")
    
    if(bootstrap_counts < 1)
        stop("bootstrap_counts has to be a positive integer")
    
    if(interval < 0 | interval > 1)
        stop("interval has to be between 0 and 1")
    
    if(bootstrap_size < 0 | bootstrap_size > 1)
        stop("bootstrap_size has to be between 0 and 1")
    
    if(!is.logical(out_string))
        stop("out_string has to be logical")
    
    if(!is.function(FUN))
        stop("FUN has to be a function")
    
    original <- FUN(x)
    
    results <- rep(NA, bootstrap_counts)
    bSize <- round(length(x) * bootstrap_size)
    for(i in 1:bootstrap_counts)
        results[i] <-  FUN(x[sample(bSize, replace = T)])
    
    qSize <- (1 - interval) / 2 
    confidence_interval <- 2*original - quantile(results, c(1 - qSize, 0 + qSize))
    
    if (out_string)
        confidence_interval <- paste(confidence_interval, collapse = ",")
    
    return(confidence_interval)
    
}

#' Gets labels of signifance for a vector of pvalues
#' @param pvalues vector of pvalues
#' @param label vector of the labels corresponding to each interval in `cutoff`
#' @param cutoff vector of increasing cutoffs
#' @param rm.na logical, if TRUE makes all NAs in the `pvalues` vectors into 1s
labelPvalues <- function(pvalues, label = c("***", "**", "*", "n.s."), cutoff = c(0, 0.001, 0.01, 0.05, 1), rm.na = T) {
    
    
    if(length(cutoff) - 1 != length(label))
        stop("the label vector has to be 1 - length of cutoff vector")
    
    if(rm.na)
        pvalues[is.na(pvalues)] <- 1
    
    if(any(pvalues < 0 | pvalues > 1))
        stop("Pvalues have to be between 0 and 1")
    
    pvalLabels <- label[findInterval(pvalues, cutoff)]
    pvalLabels[is.na(pvalLabels)] <- label[length(label)]
    
    return(pvalLabels)
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

trim <- function(x) { 
		
	# Trims white space from a character vector
	
	if(!is.character(x))
		stop("x has to be string or a character vector")

	return(gsub("\\s+", "", x))
	
}

grepTempFile <- function(x, pattern, tempLocation = "."){
	        
	# Creates a new file based of x only with lines containing
	# the specified pattern(s)
	#
	# WARNING: uses unix grep
	#   
	# x - string - path to file
	# pattern - vector - patterns to select in file

	patternArg <- paste0("-e ", paste0(pattern, collapse = " -e "))
	outFile <- file.path(tempLocation, paste0(basename(x), ".temp", sample(1:1000, 1)))

	system(paste("grep", patternArg, x, ">", outFile))

	return(outFile)

}

rankitNormalize <- function(x, IND = 1) {

    # Normalizes rows (IND = 1) or columns (IND = 2)
    # to a quantile standard normalization (i.e. rankit)

    stopifnot(is.matrix(x))
    stopifnot(is.numeric(x))

    x <- apply(x, IND, function(x) qnorm((rank(x) - 0.5) / length(x)))
    if(IND == 1)
        x <- t(x)

    return(x)

}

sourceDir <- function (path, pattern = "\\.[rR]$", env = NULL, chdir = TRUE) {
    
    # Sources all files in a folder using relative paths 
    
    files <- sort(dir(path, pattern, full.names = TRUE))
    lapply(files, source, chdir = chdir)
}


