ensemblToAlias <- function(x, organism = "hsapiens_gene_ensembl"){
	require("biomaRt")
	ensembl <- useMart("ensembl")
	ensembl <- useDataset(organism,mart=ensembl)
	
	output <- getBM(filters = "ensembl_gene_id", values = x, attributes = c("ensembl_gene_id","hgnc_symbol"), mart = ensembl)
	genes <- vector(mode = "character", length = length(x))
	names(genes) <- x
	genes[ output$ensembl_gene_id ] <- output$hgnc_symbol
	
	return(genes)
}

aliasToEnsembl <- function(x, organism = "hsapiens_gene_ensembl"){
	require("biomaRt")
	ensembl <- useMart("ensembl")
	ensembl <- useDataset(organism,mart=ensembl)
	
	output <- getBM(filters = "hgnc_symbol", values = x, attributes = c("ensembl_gene_id","hgnc_symbol"), mart = ensembl)
	genes <- vector(mode = "character", length = length(x))
	names(genes) <- x
	genes[ output$hgnc_symbol ] <- output$ensembl_gene_id
	
	return(genes)
}

