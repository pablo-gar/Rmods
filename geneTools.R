library("biomaRt")

ensemblToAlias <- function(x, organism = "hsapiens_gene_ensembl"){
	ensembl <- useMart("ensembl")
	ensembl <- useDataset(organism,mart=ensembl)
	
	output <- getBM(filters = "ensembl_gene_id", values = x, attributes = c("ensembl_gene_id","hgnc_symbol"), mart = ensembl)
	genes <- vector(mode = "character", length = length(x))
	names(genes) <- x
	genes[ output$ensembl_gene_id ] <- output$hgnc_symbol
	
	return(genes)
}

aliasToEnsembl <- function(x, organism = "hsapiens_gene_ensembl"){
	ensembl <- useMart("ensembl")
	ensembl <- useDataset(organism,mart=ensembl)
	
	output <- getBM(filters = "hgnc_symbol", values = x, attributes = c("ensembl_gene_id","hgnc_symbol"), mart = ensembl)
	genes <- vector(mode = "character", length = length(x))
	names(genes) <- x
	genes[ output$hgnc_symbol ] <- output$ensembl_gene_id
	
	return(genes)
}

#' Returns the coordinates of a vector of genes as a data.frame
#' coordinates are 1-based
#' @param x vector with gene id/names
#' @param organism - e.g.  "hsapiens_gene_ensembl"
#' @param geneType e.g. "hgnc_symbol" or " "ensembl_gene_id"
getGeneCordinates  <- function(x, organism = "hsapiens_gene_ensembl", geneType = "hgnc_symbol") {
    
    stopifnot(is.character(x))
    x <- unique(x)
    
	ensembl <- useMart("ensembl")
	ensembl <- useDataset(organism, mart=ensembl)
	output <- getBM(filters = geneType, values = x, attributes = c(geneType, "chromosome_name", "start_position", "end_position", "strand"), mart = ensembl)
    
    # only main chromosomes
    output <- output[ output$chromosome_name %in% c(1:22, "X", "Y"), ]
    output <- output[ !duplicated(output[,geneType]), ]
    
    rownames(output) <- output[, geneType]
    output <- output[x, ]
    
    rownames(output) <- x
    
    return(output)
    
}

