######################################
## Analyzes the results of GWASbyGene.r
#
# Example:
# Rscript resultStats.r ~/scripts/postSNVanalyses/GWASglobalSusceptibility/GWASbyGene/results.RData /scratch/users/paedugar/uvProject/SNVfiles/mutCount/geneMaps/phenoMatrix/phenoMatrix_relativeCountWithinRange_C\>T_FC.txt


source("~/scripts/rModules/GWAS.r")
source("~/scripts/rModules/fileManagement.r")
source("~/scripts/rModules/superApply.r")


########
## METHODS


## CALCULATING FDRS AT DIFFERETN PVALUES FOR TWO SETS OF PVALUES, REAL AND PERMUTAED

getLowerTail <- function(x, values) {
	
	# Gets the sum of elements in x that are less than each element in values
	# Can handle repeated and unsorted elements in values
	
	# Stores original info for latter rearrangment
	original <- values
	
	# Sorts and unique
	values <- unique(values)
	sortedValues <- sort(values)
	sortedValues <- c(-Inf, sortedValues, Inf)
	
	# Makes the counting
	counts <-  cumsum(table(findInterval(c(sortedValues, x),sortedValues) ) - 1)
	# Ignonres last element
	counts <- counts[1:length(values)]
	
	# Reorder according to original vector
	names(counts) <- order(values)
	counts <- counts[sort(as.numeric(names((counts))))]
	names(counts) <- as.character(values)
	
	counts <- counts[as.character(original)]
	
	return(counts)
	
}

FDRcalculation <- function(realPvals, permPvals) {
	
	# Gets percentage of permPvals that less than each realPvalue
	expectedPercentage <- getLowerTail(permPvals, realPvals) / length(permPvals)
	
	# Gets expected number of positive (based on permutations) for each pvalue
	expectedFalsePos <- expectedPercentage * length(realPvals)
	
	# Calculate FDR, falsePos / real Pos
	FDR <- expectedFalsePos/(1:length(realPvals))

	# Re order according to original vector
	names(FDR) <- order(realPvals)
	FDR <- FDR [sort(as.numeric(names(FDR)))]
	names(FDR) <- as.character(realPvals)
	
	return(FDR)
}

getFDRMatQTL <- function(matQTL, permDir, FDR_cutoff = 0.02) {
	
	# This a specific function only used in the GWASbyGene pipeline
	# matQTL is a single reseult of matrixQTL containing ALL pvalues
	# and an extra slot with the id of the gene
	# permDir is the directory where permutations of each gene are saved
	# as individual RData files this files were created with=
	# lmMatGRangePhenomatrixDeletme from the GWAS pipeline
	
	gene <- matQTL$geneName
	permFile <-joinPath(permDir, paste0(gene, ".RData")) 	
	
	if(file.exists(permFile)) {
		load(permFile)
	} else {
		return()
	}
	
	real <- matQTL$all$eqtls
	perm <- lmresults$all$eqtls$pvalue
	
	#browser()
	FDR <- FDRcalculation(real$pvalue, perm)
	
	positiveCount <- FDR <= FDR_cutoff
	if(sum(positiveCount) > 0) {
		real <- real[positiveCount,]
		real$FDR <- FDR[positiveCount]
		return(real)
	} else {
		return()
	}
	
}
	
# FDR ESTIMATION BASED ON A SINGLE VECTOR OF PVALUES

getFDRvector <- function(pvals) {
	
	# Estimates FDR for each pvalue in the vector based on the expectation
	# of a uniform distribution of pvalues
	
	if(sum(pvals < 0 | pvals > 1) > 0)
		stop("Pvalues have to be between 0 and 1")
	
	# pvalues have to be sorted
	
	pvalUnique <- unique(sort(pvals))
	pvalCounts <- table(pvals)
	pvalCounts <- cumsum(pvalCounts)
	
	expectedItems <- length(pvals)*pvalUnique
	names(expectedItems) <- as.character(pvalUnique)
	
	#browser()
	FDRs <- expectedItems / pvalCounts
	FDRs[ FDRs > 1 ]  <- 1
	
	names(FDRs) <- as.character(names(expectedItems))
	FDRs <- FDRs[as.character(pvals)]
	names(FDRs) <- names(pvals)
	
	
	return(FDRs)
	
}

plotPvalFDR <- function(pvals, poinType = 19, pointSize = 0.3, ...) {
	
	FDR <- getFDRvector(pvals)
	plot (sort(pvals), FDR[ order(pvals) ], type = "l", xlab = "p-value", ylab = "FDR", ...)
	points(sort(pvals), FDR[ order(pvals) ], pch = poinType, cex = pointSize)
	
}
