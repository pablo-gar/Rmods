printTime <- function(x = "") {
	# Prints to console the current date and time followed by
	# the message(s) in x
	flush.console()
	cat(as.character(Sys.time()), x)
}
