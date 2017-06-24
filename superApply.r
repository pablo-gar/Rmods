source("~/scripts/Rmods/misc.r")

testF <- function (x){
	Sys.sleep(120)
	return(T)
}

source("~/scripts/rModules/submitSLURM.r")

#aaa <- superApply(1:2000, rep, times = 3, tasks = 10, workingDir = "~/deletemeR", clean = T, clusterPar = "--partition=hbfraser --time=60")
#aaa <- superApply(1:10, testF, tasks = 10, workingDir = "~/deletemeR", clean = F, clusterPar = "--partition=hbfraser --time=15:00")
#aaa <- superApply(1:10, testF, tasks = 10, workingDir = "~/deletemeR", clean = F, queue="hbfraser", time="15:00")


#superApply <- function(x,FUN, ...,  tasks = 1, workingDir, clean = F, clusterPar = "--partition=hbfraser", extraScriptLines = ""){
superApply <- function(x,FUN, ...,  tasks = 1, workingDir, extraScriptLines = "", clean = F, queue = "hbfraser", time = NULL, qos = NULL, mem = NULL){

	dir.create(workingDir, showWarnings = F, recursive = T)
	setwd (workingDir)
	system(paste0("rm ", file.path(workingDir, "*")))
	
	# Making unique ids	
	idPrefix <- paste0(c("sAp_", sample(letters, size=3), sample(0:9,size=1), "_"), collapse = "")

	#####
	# Parsing x, is it vector, list? or is it number of repetitions (i.e. x is just a number)?
	FUN <- match.fun(FUN)
	if(!is.vector(x)){
		x <- as.list(x)
		times <- length(x)
	}else{
		if(length(x) == 1 & is.numeric(x)){ # This will make apply ignore x and will execute FUN x times with ... arguments.
			times <- x			# It requires a FUN that does nothing with its first argument
			ignoreX <- TRUE
		}else{
			times <- length(x)
		}
	}
	
	#####
	# Creates indexes to partition data for parallel processing
	jobsPerTask <- ceiling(times/tasks)
	iStart <- seq(1, times, jobsPerTask)
	iEnd <- seq (jobsPerTask, times, jobsPerTask) 
	if(iEnd[length(iEnd)] < times) 
		iEnd <- c(iEnd,times)
	
	# Submits jobs
	jobList <- vector(mode = "character",length = length(iStart))
	
	printTime("Submmiting parallel Jobs\n")
	
	for(i in 1:length(iStart)){
		currentX <- x[iStart[i]:iEnd[i]]
		jobList[i] <- paste0(idPrefix, i)
		#submitLapplySlurm(currentX, FUN, ..., workingDir = workingDir, id = jobList[i], clusterPar = clusterPar, extraScriptLines = extraScriptLines)
		submitLapplySlurm(currentX, FUN, ..., workingDir = workingDir, id = jobList[i], extraScriptLines = extraScriptLines, queue = queue, time = time, qos = qos, mem = mem)
		#Sys.sleep(0.8)
	}
	
	printTime(" All parallel jobs submitted. Waiting for them to finish\n")
	
	#####
	# Waiting for jobs to finish
	expectedOutFiles <- paste0(jobList, ".outRData")
	expectedOutVariables <- paste0("output_", jobList)
	jobList <- paste0(jobList, collapse = ",")
	repeat{
		
		#Checking outdir for expected files
		status <- checkFiles(expectedOutFiles, workingDir)
		Sys.sleep(5)
		# Printing info 
		jobStates <- getStateCount(jobList)
		cat("\r", as.character(Sys.time()), " Expected number of Jobs =", length(iStart), "Not-yet-found =", status$remaining,"--- Cluster Status: " )
		for(state in names(jobStates))
			cat(state, "=", jobStates[state], "|")
		
		if (status$status){
			#Printing info again because delay causes misleading printing info
			jobStates <- getStateCount(jobList)		
			cat("\n", as.character(Sys.time()), "DONE! Expected number of Jobs =", length(iStart), "Not-yet-found =", status$remaining,"--- Cluster Status: " )
			for(state in names(jobStates))
				cat(state, "=", jobStates[state], "|")
			cat("\n")
			break	
		}
		
	}
	
	printTime("Jobs done\n")
	
	#####
	# Collecting output from individual calls
	printTime("Merging parellel results\n")
	supperApplyResults <- mergeListDir (expectedOutFiles, expectedOutVariables, workingDir)
	printTime("Merge done\n")
	
	if(clean)
		system(paste0("rm ", file.path(workingDir, "*")))	
	
	return(supperApplyResults)
		
}


#submitLapplySlurm <- function(x, FUN, ..., workingDir, id,  clusterPar, extraScriptLines = ""){
submitLapplySlurm <- function(x, FUN, ..., workingDir, id, extraScriptLines = "", queue = "hbfraser", time = NULL, qos = NULL, mem = NULL){
	
	dir.create(workingDir, showWarnings = F)
	setwd (workingDir)
	
	# Setting file and var names
	#xDataFile <- file.path(workingDir, paste0(id, ".xRData"))
	#funDataFile <- file.path(workingDir, paste0(id, ".funRData"))
	#parsDataFile <- file.path(workingDir, paste0(id, ".parsRData"))
	outDataFile <- file.path(workingDir, paste0(id, ".outRData"))
	dataFile <- file.path(workingDir, paste0(id, ".applyRData"))
	
	#Saving RData files used in script
	#save(x, file = xDataFile)
	#save(FUN, file = funDataFile)	
	#save(pars, file = parsDataFile)
	pars <- list(...)
	save(x,FUN,pars, file = dataFile)
	rm(x,FUN,pars)
	gc()
	
	#Making script to be submmited
	tempScriptFile <- file.path(workingDir, paste0(id, ".parallelBatch"))
	tempScript <- c(extraScriptLines,
				#paste0("load('", xDataFile, "')"),
				#paste0("load('", funDataFile, "')"),
				#paste0("load('",  parsDataFile, "')"),					
				paste0("load('",  dataFile, "')"),					
				paste0("output_", id, " <- do.call( lapply, c(list(X = x, FUN = FUN), pars))" ),
				paste0("save(output_", id, ", file='", outDataFile, "')")
			)
	
	RscriptFile <- file.path(workingDir, paste0(id, ".Rscript"))
	writeLines (tempScript, RscriptFile)
	cmds <- c("module load R/3.3.0", paste0("Rscript --vanilla ", RscriptFile))
	clusterSubmit(id, workingDir, cmds, queue = queue, time = time, qos = qos, mem = mem, nodes = 1, proc = 1)
	
	#sallocCmd <- paste0("salloc --nodes=1 --quiet --ntasks=1 ", clusterPar," --job-name=", id, " Rscript --vanilla ", RscriptFile)
	#system(sallocCmd, wait = F )
	
}

mergeListDir <- function(files, varNames, workingDir){
	finalF <- list()
	for (i in 1:length(files)){
		load(file.path(workingDir,files[i]))
		finalF <- c(finalF, eval(parse(text = varNames[i])))
	}
	return(finalF)
}

#mergeListDir <- function(files){
#	finalF <- list()
#	for (i in 1:length(files)){
#		flush.console()
#		cat(i, "\n")
#		load(file.path(files[i]))
#		varName <- paste0("output", "_", gsub(".outRData", "", files[i]))
#		#aaa <- lapply(eval(parse(text = varName)), function(x) x$all$eqtls[,c("snps", "pvalue")])
#		finalF <- c(finalF, aaa)
#		rm(list = c(varName, "aaa"))
#		gc()
#	}
#	return(finalF)
#}


getStateCount <- function(jobNames) {
	jobStates <- system(paste0("sacct --noheader --parsable2 --format=JobID,State --name=", jobNames), intern = T )
	jobStates <- jobStates[ !grepl("\\..+", jobStates) ]
	jobStates <- gsub(".+\\|(.+)", "\\1", jobStates)
	return(table(jobStates))
}

checkFiles <- function (x,workingDir){
	remaining <- sum( !x %in% list.files(workingDir)) 
	status <- ifelse(remaining == 0, TRUE, FALSE)
	
	return(list(status = status, remaining = remaining))
}

