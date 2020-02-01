#' A vareity of functions to deal with SNPs

suppressMessages({
    library('GenomicRanges')
    library('tools')
})

.genomes_supported=c('GRCh37', 'GRCh38')

#' systemSubmit
#' 
#' Internal function for the rSubmitter package
#' Tries executing a command up to n times while the execution returns a
#' non-zero exit status.
#' Useful when trying to automatically submmit a job and the scheduler has problems,
#' then this will try several times if submission fails for any reason
#'
#' @param command Character - system command to execute
#' @param n Integer - number of times to try executing command in case it returns a non-zero exit status
#' @param wait Integer - time in seconds to wait before trying executing the command again
#' @param ignore.stdout Logical - if TRUE it won't return the standard output of execution
#' @param ignore.stderr Logical - if TRUE it won't return the standard error of execution
#' @param intern Logical - see ?system
#' @param stopIfFailed Logical, if TRUE throws error after all tries have failed, else throws a warning
#' @param ... to be passed to system()
#' 
#' @return system return object - see ?system
systemSubmit <- function(command, n = 5, wait = 5, ignore.stdout = TRUE, ignore.stderr = F, intern = TRUE, stopIfFailed = TRUE, verbose = F, ...) {
    stopifnot(is.character(command), length(command) == 1)
    
    count = 0
    while(count < n) {
        
        commandResult <- suppressWarnings(system(command, intern = intern, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr, ...))
        exitStatus <- attr(commandResult, "status")
        
        if(is.null(exitStatus))
            break
        if(exitStatus == 0)
            break
        
        count = count + 1
        if(verbose) {
            cat("\nSomething went wrong with the system execution, trying again in ", wait, " seconds\n")
            cat("    Error observed: ", exitStatus, "\n\n\n")
        }
        Sys.sleep(wait)
    }
    
    if(count == n) {
        errorString <- paste0("\n\nFailed to execute after ", n, " tries:\n", 
                              "   Command: ", command, "\n",
                              "   Last output: ", commandResult, "\n",
                              "   Last exit code: ", exitStatus, "\n"
                             )
        if(stopIfFailed) { 
            stop(errorString)
        } else {
            warning(errorString)
        }
    }
    
    return(commandResult)
    
}

#' Find snps in LD
#' 
#' From a data frame that has the columns, chromosome and rsid
#' it retrieves any snps that may be in ld with the rsids
#' It makes and external call to PLINK
#'
#' @param x data.frame - contains the columns chromosoe and rsid
#' @param chrCol numberic - index of chromsome column in x
#' @param rsidCol numberic - index of rsids column in x
#' @param ensembl_chr logical - if TRUE chromosme format is juts a number, else it is chr1, chr2, etc
#' @param plink_data_location character - path to folder containing the input plink data divided by chromosomewith the following name formatting (regex) .*chr\w+.*\.[bed|bim|fam], 
ld_rsid <- function(x, chrCol, rsidCol, ensembl_chr=F, plink_data_location='/scratch/users/paedugar/plink_data/1000genomes/phase3/CEU/bed', memory='4000'){
    
    # correcting names
    if(ensembl_chr)
        x[,chrCol] <- paste0('chr', x[,chrCol, drop=T])
    
    chrs <- unique(x[,chrCol, drop=T])
    
    # Finding snps in LD for each chromosome
    results <- list()
    for (chr in chrs) {
        
        message()
        
        current_x <- x[ x[, chrCol] == chr,]
        rsids <- current_x[, rsidCol, drop=T]
        rsids <- paste(rsids, collapse=', ')
        current_ld <- .ld_rsid_helper(chr, rsids, plink_data_location=plink_data_location, memory=memory)
        if(!is.null(results)) 
            results[[chr]] <- current_ld
                                      
    }
    
    results <- do.call(rbind, results)
    
    return(results)
}


#' returns data frame with columns: input snps, snps in ld, ld score
#' if no snps in LD returns NULL
.ld_rsid_helper <- function(chr, rsids, plink_data_location, memory='8000') {
    
    input_files <- list.files(plink_data_location, full.names=T, pattern=paste0(chr, '\\b.*(bed|bim|fam)'))
    
    input_prefix <- unique(file_path_sans_ext(input_files))
    
    if(length(input_files) == 0)
        stop(paste('Plink input files for chr', chr, 'not found. Make sure that plink location and chromosome nomeclature are correct'))
    
    if(length(input_prefix)!=1)
        stop('Something went wrong, possible multiple chromosome files match to single chromsome')
    
    out_prefix <- tempfile()
    curcall <- systemSubmit(command = paste("plink --bfile", input_prefix, "--memory", memory, "--r2 --ld-snps", rsids, "--ld-window 99999 --ld-window-kb 500 --ld-window-r2 .8 --out", out_prefix), stopIfFailed=F)
    
    result_file <-  paste0(out_prefix, '.ld')
    
    if(!file.exists(result_file)) {
        return (NULL)
    } else {
        results <- read.table(result_file, header=T, stringsAsFactors=F)
        if(nrow(results) > 1) {
            results <- results[,c('SNP_A', 'SNP_B', 'R2')]
        } else {
            return(NULL)
        }
    }
    file.remove(result_file)
    return(results)
    
        
}

#' Converts gtex snp ids to their corresponding rsids in a character vector format
#' @param x character vector - gtex snps
#' @param genome_build character - genome build [GRCh37|GRCh38]
gtexSnp_to_rsid <- function(x, genome_build='GRCh37') {
    
    rsid <- gtexSnp_to_rsid_dataframe(x=x, genome_build=genome_build)
    rsid <- rsid[,'RefSNP_id']
    return(rsid)
    
}

#' Converts gtex snp ids to their corresponding rsids in a data frame that contains coordinates as well
#' @param x character vector - gtex snps
#' @param genome_build character - genome build [GRCh37|GRCh38]
gtexSnp_to_rsid_dataframe <- function(x, genome_build='GRCh37') {
    
    if(!genome_build %in% .genomes_supported)
        stop('Genome build not supported')
    
    # convert to data frame for granges
    snp_granges <- geno_id_to_string(x, format_out = "grange_ensembl")
    # Get rsids
    if (genome_build == 'GRCh37') {
        suppressMessages(library('SNPlocs.Hsapiens.dbSNP144.GRCh37'))
        snp_rsid <- as.data.frame(snpsByOverlaps(SNPlocs.Hsapiens.dbSNP144.GRCh37, makeGRangesFromDataFrame(snp_granges)))
    } else if (genome_build == 'GRCh38'){
        suppressMessages(library('SNPlocs.Hsapiens.dbSNP151.GRCh38'))
        snp_rsid <- as.data.frame(snpsByOverlaps(SNPlocs.Hsapiens.dbSNP151.GRCh38, makeGRangesFromDataFrame(snp_granges)))
    }
    
    # Eliminate duplicates and order
    ids <- paste0(snp_rsid$seqnames, ".", snp_rsid$pos)
    snp_rsid <- snp_rsid[!duplicated(ids),]
    rownames(snp_rsid) <- ids
    rsid <- snp_rsid[paste0(snp_granges$chr, ".", snp_granges$start),,drop=F]
    rsid <- rsid[!is.na(rsid[,1]), , drop=F]
    
    return(rsid)
    
}

#' Converts gtex snp ids to several string or granges
#'
#' if format_out is 'grange' or 'grange_ensembl' will return a data.frame ready for GenomicRanges::makeGRangesFromDataFrame() with but in the latter 
#' chromosome names are numeric values.
#'
#' @param x character vector - gtex snps
#' @param format_out character - format of output conversion [string|grange|grange_ensembl]
gtexSnp_to_string <- function(x, format_out = "string") {
    
    x <- strsplit(x, "_")
    if (format_out == "string") {
        x <- sapply(x, function (x) paste0("chr", x[1], ":", x[2], " / Ref: ", x[3], " / Alt: ", x[4]))
    } else if (format_out == "grange") {
        x <- lapply(x, function(x) data.frame(chr = paste0("chr", x[1]), start = as.numeric(x[2]), end = as.numeric(x[2]) + 1))
        x <- do.call(rbind, x)
    } else if (format_out == "grange_ensembl") {
        x <- lapply(x, function(x) data.frame(chr = x[1], start = as.numeric(x[2]), end = as.numeric(x[2]) + 1))
        x <- do.call(rbind, x)
    }
    return(x)
}
