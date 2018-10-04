library("biomaRt")
library("org.Hs.eg.db")
library("topGO")

#' performGO
#' @param gene a named numeric vector, names are gene ids and values are used for selection criteria of target genes
#'
#' @return a list the topGOdata and fisher resuts
performGO <-  function(genes, type = "Ensembl", ontology = "BP", gene_selection_function = function(p) p < 0.05) {
    
    GOdata <- new("topGOdata", ontology = "BP", allGenes = genes, geneSel = gene_selection_function, annot = annFUN.org, mapping="org.Hs.eg.db", ID=type)
    fisher <- runTest(GOdata, algorithm = "classic", statistic = "fisher")
    
    return(list(GOdata = GOdata, fisher = fisher))
}


#' get_GO_results
#' takes output of perfomGO and returns a data.frame with GO terms and their pvalues
get_GO_results <- function(x, test = "fisher", fdr_signif = F, bonf_signif = T, n = NULL) {
        
    if(sum(fdr_signif, bonf_signif, !is.null(n)) > 1)
        stop("Only one of these arguments can be TRUE, fdr_signif, bonf_signif, n")
        
    pvals <- score(x[[test]])
        
    if (is.null(n)) {
        result <- GenTable(x$GOdata, x[[test]], numChar = 1000) 
    } else {
        result <- GenTable(x$GOdata, x[[test]], numChar = 1000, topNodes = n) 
    }   
        
            
    result$p_bonf <- p.adjust(pvals, method = "bonferroni")[result$GO.ID]
    result$fdr <-  p.adjust(pvals, method = "fdr")[result$GO.ID]
        
    if(fdr_signif)
        result <- result[result$fdr < 0.05,]
    if(bonf_signif)
        result <- result[result$p_bonf < 0.05,]
        
    return(result)
        
}

