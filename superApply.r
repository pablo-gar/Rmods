#--------------------------------------------------------------------
# DESCRIPTION
# 
# supperApply, it mimics the functionality of lapply but implemented
# in a way that each iteration of the apply is submmitted as an individual
# job to a SLURM cluster. Hence emulating a parellel behaivor in apply
#
# Each job batch, err, out, and script files are stored in a temporary folder. Once
# all jobs have been submmitted, the function waits for them to finish. Once the jobs
# are done it compiles all the results into list and returns them, therefore fully
# mimicking the apply behaivor.
# 
# Author: Pablo Garcia
# 
# Date: September, 2016
# 
# Requirements:
#	Rmods/misc.r
#	Rmods/submitSLURM.r
#
# IMPORTANT 
# 	To load this script use
# 	source("path/To/Rmods/superApply.r", chdir = T)
#--------------------------------------------------------------------

source("./misc.r")
source("./submitSLURM.r")
source("./fileManagement.r")


#--------------------------------------------------------------------
# GLOBAL

# Prefix for file generated for superApply
SAP_PREFIX <- "sAp_"

# Wait time to re check job status when they have been submitted
SAP_WAIT_TIME <- 10


# Completed message from scheduler
SCH_COMPLETED = "COMPLETED"

# Error messages from scheduler
SCH_FAIL_MSG <- "FAILED"
SCH_TIMEOUT_MSG <- "TIMEOUT"
SCH_CANCEL_MSG <- "CANCELLED"
SCH_NODEFAIL_MSG <- "NODE_FAIL"
#SCH_PREEMPTED_MSG <- "PREEMPTED"

SCH_ERROR_MSG <- c(SCH_FAIL_MSG, SCH_TIMEOUT_MSG, SCH_CANCEL_MSG, SCH_NODEFAIL_MSG)

# Name of file containing job information in case one or more jobs fail (will be stored in working dir of superApply())
FAILED_LOG_FILE <- "1_LOG_JOBS.txt"
#--------------------------------------------------------------------


testF <- function (x){
	Sys.sleep(1)
	return(T)
}

#aaa <- superApply(1:30, rep, tasks = 10, workingDir = "~/deletemeR", clean = F, time = "60", mem = "1G")


superApply <- function(x, FUN, ...,  tasks = 1, workingDir, extraScriptLines = "", clean = F, queue = "hbfraser", time = NULL, qos = NULL, mem = NULL){

	# Main function that emulates apply but with parallel processing behaivor.
	# It first divides elements of x into buckets of length(x)/tasks, then
	# uses each bucket as individual elements where apply() will be used with FUN 
	# as individual SLURM submissions.
	# The submission process goes as follow for each bucket:
	#   - Saves each bucket data as and RData file with an associated id name
	#	- Creates an R script that will load the bucket RData and execute apply() and FUN. 
	#     It will save the results as separate result RData file
	#	- Creates a batch script that submmits the R script
	#	- Submmits the job using the same id
	# 
	# Oncer all Jobs have been submmitted and finished it will compile all the result RData files
	# from the inidividual jobs into a single list.
	# This is the list to be returned
	#
	# ARGS:
	# x - vector/list - FUN will be applied to the elements of this. If x is and integer of length one, FUN will be executed x times with pars "..."
	# FUN - function - function to be applied to each element of x. 
	# ... - further arguments of FUN
	# tasks - integer - number of individual parallel jobs to execute
	# workingDir - string - path to folder that will contain all the temporary files needed for submission, execution,
    #                       and compilation of inidivudal jobs
	# extraScriptLines - string - extra code to be added to all of the individual parallel jobs
	#                             IMPORTANT: if FUN requires any library they have to be included here (e.g. extraScriptLines = "library(reshape); library(GenomicRanges)"
	# time - string - time allocated to each individual job, format "hh:mm:ss"
	# qos - string - SLURM qos
	# mem - string - memory allocated to each individual job, e.g. "10G", "10000"
	#
	# Return - list - results of FUN applied to each element in x
	
	FUN <- match.fun(FUN)
	
	# Getting indeces to partition X into different tasks (i.e. individual jobs)
	partitionIndeces<- getPartitionIndeces(x, tasks = tasks)
	
	# Submmitting individual jobs
	printTime("Submmiting parallel Jobs\n")
	
	jobs <- submitJobs(x, FUN, ..., partitionIndeces = partitionIndeces, workingDir = workingDir, extraScriptLines = extraScriptLines, queue = queue, time = time, qos = qos, mem = mem)
	
	# Waiting for jobs to finish
	expectedOutFiles <- paste0(jobs$jobName, ".outRData")
	expectedOutVariables <- paste0("output_", jobs$jobName)
	expectedNjobs <- nrow(jobs)
	jobList <- paste0(jobs$jobId, collapse = ",")
	
	repeat{
		
		#Checking outdir for expected files
		status <- checkFiles(expectedOutFiles, workingDir) # THERE IS A SMALL BUG WITH THIS ONE
		
		# Printing info and stop if an error is found
		jobStates <- getStateCount(jobList)
		stopIfFailedJobs(jobStates, workingDir = workingDir)
		printJobInfo(jobStates$stateFrequency, status)
		
		completed <- jobStates$stateFrequency[SCH_COMPLETED]
		
		# If all jobs finished break the loop
		#if (status$finished){
		if (!is.na(completed)){
			if (completed == expectedNjobs) {
				break	
			}
		}
		
		Sys.sleep(SAP_WAIT_TIME)
	}
	
	cat("\n")
	printTime("Jobs done. Final State:\n")
	jobStates <- getStateCount(jobList)		
	printJobInfo(jobStates$stateFrequency, status)
	cat("\n")
	
	# Collecting output from individual calls
	printTime("Merging parellel results\n")
	supperApplyResults <- mergeListDir (expectedOutFiles, expectedOutVariables, workingDir)
	printTime("Merge done\n")
	
	# Removing jobs files if desired
	if(clean)
		system(paste0("rm ", file.path(workingDir, "*")))	
	
	return(supperApplyResults)
		
}



getPartitionIndeces <- function(x, tasks = tasks) {
	
	# Helper of superApply
	# Creates a list  with slots, containing the start and end indeces 
	# corresponding to the partitions of x required to run the number of parallel tasks
	#
	# Parsing x, is it vector, list? or is it number of repetitions (i.e. x is just a number)?
	# This just to calculate the number of times the FUN has to be executed
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
	
	# Creates indexes to partition data for parallel processing
	jobsPerTask <- ceiling(times/tasks)
	iStart <- seq(1, times, jobsPerTask)
	iEnd <- seq (jobsPerTask, times, jobsPerTask) 
	if(iEnd[length(iEnd)] < times) 
		iEnd <- c(iEnd,times)
	
	# Returns partition indices
	result <- list(iStart = iStart, iEnd = iEnd)
	return(result)

}	

submitJobs <- function(x, FUN, ..., partitionIndeces, workingDir, extraScriptLines, queue, time, qos, mem) {
	
	# Helper of superApply
	# Submits multiple jobs from the partions of x created in get Partition Indeces
	#
	# x - list/vector - data to be partition
	# FUN - function - function to be applied to each element of x
	# partitionIndeces - list - output of getPartitionIndeces()
	#
	# RETURNS a data.frame of two columns, jobName and jobId
	
	
	# Cleaning and or creating workind dir for submission
	idPrefix <- SAP_PREFIX
	dir.create(workingDir, showWarnings = F, recursive = T)
	system(paste0("rm ", joinPath(workingDir, paste0(idPrefix, "*"))), ignore.stdout = T, ignore.stderr = T)
	
	# Making unique ids	for each submission
	idPrefix <- paste0(c(idPrefix, sample(letters, size=3), sample(0:9,size=1), "_"), collapse = "")
	
	iStart <- partitionIndeces$iStart
	iEnd <- partitionIndeces$iEnd
	jobs <- data.frame(jobName = rep("", length(iStart)), jobId = "", stringsAsFactors = F)
	
	for(i in 1:length(iStart)){
		
		# Submitting jobs
		currentX <- x[iStart[i]:iEnd[i]]
		jobs[i, "jobName"] <- paste0(idPrefix, i)
		
		jobId <- submitLapplySlurm(currentX, FUN, ..., workingDir = workingDir, id = jobs[i, "jobName"], extraScriptLines = extraScriptLines, queue = queue, time = time, qos = qos, mem = mem)
		cat(jobId, "\n")
		
		# Saving ids
		jobs[i, "jobId"] <- jobId
	}
	
	return(jobs)
}


submitLapplySlurm <- function(x, FUN, ..., workingDir, id, extraScriptLines = "", queue = "hbfraser", time = NULL, qos = NULL, mem = NULL){
	
	# Helper of superApply
	# Takes a vector/list x, a function FUN and extra paramaters (...) and submits an Rscript
	# that executes lappy in x using FUN, saves the scripts, results and slurm fil;e in workingDir  
	# 
	# x - vector/list - data to which lapply will be executed
	# FUN - function - function to be applied to x
	# ... - extra paramaters passed to FUN
	# extraScriptLines - string - lines to be added at the beginning of the Rscript before lapply (useful to load packages)
	# queue - string - queue in SLURM for job submission
	# time - string - estimated time for lapply to finish, format "hh:mm:ss"
	# qos - string - SLURM qos
	# mem - string - estimated memory requiered by lapply execution, format e.g "10G"
	
	# Setting file and var names
	outDataFile <- file.path(workingDir, paste0(id, ".outRData"))
	dataFile <- file.path(workingDir, paste0(id, ".applyRData"))
	
	#Saving RData files used in script
	pars <- list(...)
	save(x, FUN, pars, file = dataFile)
	rm(x, FUN, pars)
	gc()
	
	#Making script to be submmited
	tempScriptFile <- file.path(workingDir, paste0(id, ".parallelBatch"))
	tempScript <- c(
					extraScriptLines,
					paste0("load('",  dataFile, "')"),					
					paste0("output_", id, " <- do.call( lapply, c(list(X = x, FUN = FUN), pars))" ),
					paste0("save(output_", id, ", file='", outDataFile, "')")
					)
	
	RscriptFile <- file.path(workingDir, paste0(id, ".Rscript"))
	writeLines (tempScript, RscriptFile)
	
	# Submitting job
	cmds <- c("module load R/3.3.0", paste0("Rscript --vanilla ", RscriptFile))
	clusterSubmit(id, workingDir, cmds, queue = queue, time = time, qos = qos, mem = mem, nodes = 1, proc = 1)
	
}

mergeListDir <- function(files, varNames, workingDir){
	
	finishedFiles <- files %in% list.files(workingDir)
	files <- files [finishedFiles]
	varNames <- varNames[finishedFiles]
	
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


getStateCount <- function(jobIds) {
	jobInfo <- system(paste0("sacct --noheader --parsable2 --format=JobID,JobName,State --job=", jobIds), intern = T )
	jobInfo <- jobInfo[grepl(SAP_PREFIX, jobInfo)]
	jobInfo <- jobInfo[ !grepl("\\..+", jobInfo) ]
	
	
	jobIds <- gsub("(.+)\\|.+\\|.+", "\\1", jobInfo) 
	jobNames <- gsub(".+\\|(.+)\\|.+", "\\1", jobInfo) 
	jobStates <- gsub(".+\\|.+\\|(.+)", "\\1", jobInfo)
	
	result <- list(
				   jobStates = data.frame(jobId = jobIds, jobName = jobNames, jobState = jobStates, stringsAsFactors = F),
				   stateFrequency = table(jobStates)
				   )
	
	return(result)
}

checkFiles <- function (x,workingDir){
	
	applyFiles <- list.files(workingDir)[grep(SAP_PREFIX, list.files(workingDir))]
	remaining <- sum( !x %in% applyFiles) 
	
	finished <- ifelse(remaining == 0, TRUE, FALSE)
	total <- length(x)
	
	
	return(list(finished = finished, remaining = remaining, total = total))
}

stopIfFailedJobs <- function(jobStates, workingDir) { 
	
	# Helper of superApply
	# Stops execution if one of the jobs failed
	# 
	# Saves a final state error file in working dir indicating which jobs failed
	#
	# jobStates - list - output of getStateCount()
	
	if( sum(names(jobStates$stateFrequency) %in% SCH_ERROR_MSG)  > 0 ) {
		write.table(jobStates$jobStates, joinPath(workingDir, FAILED_LOG_FILE), sep = "\t", row.names = F, quote = F)
		cat("\n")
		stop(paste0("One or more jobs failed, take a look at ", joinPath(workingDir, FAILED_LOG_FILE)))
	}
	
	
}
	

printJobInfo <- function(jobStatesFreq, status) {
	
	# Helper of superApply
	# Prints the current state of submitted jobs in the format
	#  Expected number of Jobs = X Not-yet-found = --- Cluster Status: COMPLETED=10|RUNNING=2
	# 
	# jobStatesFreq - vector/table - slot "stateFreqency" of getStateCount() output 
	# status - list - output of check files
	
	# Print expected number of jobs
	printTime(carriageReturn = T, x = paste0("Expected number of Jobs = ", status$total, "; Not-yet-found = ", status$remaining," --- Cluster Status: "))
	
	# Print how many have been completed, running, etc
	for(state in names(jobStatesFreq))
		cat(state, "=", jobStatesFreq[state], "|")
}
