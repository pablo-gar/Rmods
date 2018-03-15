library(ggplot2)
library(reshape)
library(gplots)
scatter <- function(dataframe, x, y, facet_x = NULL, facet_y = NULL, scales = "free", labelSize = 4, labelRound = 2, regression = F, nrowFactor = 1, ncolFactor = 1) {
        # Makes a scatter plot from a data frame with the following columns
        # x = string; column with data for x axis
        # y = string, column with data for y axis
        # facet_x = factor, column with factor to subdivide the data and plot in different columns
        # facet_y = factor, column with factor to subdivide the data and plot in different rows
        #
        # dataframe = data.frame, with the columns indicated above
        #
        # ... further parameters for facet_wrap
        #
        # returns a plot object of ggplot2
        
        require(ggplot2)
        
        #--------------------------
        # Checking parameters 
        #--------------------------
        
        if(!is.data.frame(dataframe)) stop ("dataframe has to be a data.frame")
        if(!is.character(x) | !is.character(y) | length(x) > 1 | length(y) > 1) stop ("x and y have to be character vectors of length one")
        if(!isCol(x, dataframe) | !isCol(y,dataframe)) stop ("x and y have to be columns of dataframe")
        
        if(!is.null(facet_x))
           if(!isCol(facet_x, dataframe)) 
               stop("facet_x has to be a column of dataframe or null")
        if(!is.null(facet_y)) 
            if(!isCol(facet_y, dataframe)) 
                stop("facet_y has to be a column of dataframe or null")
        
        
        #-----------------------------
        # Getting correlation strings
        #-----------------------------
        
        Params <- list(dataframe = dataframe, x = x, y = y, labelRound = labelRound)
        
        if (is.null(facet_y) & is.null(facet_x)) {
            Params <- c(Params, list(cat = NULL))
            facetForm <- ""
        } else if(is.null(facet_y)) {
            Params <- c(Params, list(cat = facet_x))
            facetForm <- paste0("~", facet_x)
            ncol <- length(unique(dataframe[,facet_x]))
            nrow <- 1 * nrowFactor
        } else if(is.null(facet_x)) {
            Params <- c(Params, list(cat = facet_y))
            facetForm <- paste0(facet_y, "~")
            ncol <- 1 * ncolFactor
            nrow <- length(unique(dataframe[,facet_y]))
        } else {
            Params <- c(Params, list(cat = c(facet_x, facet_y)))
            facetForm <- paste0(facet_y, "~", facet_x)
            ncol <- length(unique(dataframe[,facet_x]))
            nrow <- length(unique(dataframe[,facet_y]))
        }
        
        corString <- do.call(getCorString, Params)
        
        corString$x <- -Inf
        corString$y <- Inf
        colnames(corString)[ (ncol(corString) - 1) : ncol(corString)] <- c(x,y)
        
        #----------------------------
        # Making plots
        #----------------------------
        p <- ggplot(dataframe, aes_string( x = x, y = y )) +
        geom_point() + 
        geom_text(aes(label = text), data = corString, hjust = 0, vjust = 1, size = labelSize) + 
        theme_bw()
        
        if(!is.null(facet_x) | !is.null(facet_y)){
            facetForm <- as.formula(facetForm)
            p <- p + facet_wrap(facetForm, scales = scales, nrow = nrow)
        }
        
        if(regression)
            p <- p + geom_smooth(method = "lm")

       #DONE
       return(p) 
        
}

isCol <- function(x, mat) {
        return (x %in% colnames(mat))
}

getCorString <- function(dataframe, x, y, cat = NULL, labelRound = 2) {
        
        # Calculates pearson correlation coefficient and pvalue between two columns
        # in a dataframe
        
        #--------------------------
        # Checking parameters 
        #--------------------------
        if(!is.character(x) | !is.character(y) | length(x) > 1 | length(y) > 1) stop ("x and y have to be character vectors of length one")
        if(!isCol(x, dataframe) | !isCol(y,dataframe)) stop ("x and y have to be columns of dataframe")
        
        if(!is.null(cat)) {
            for(i in cat) { 
                if(!isCol(i, dataframe)){ stop ("catgories have to be columns of dataaframe")}
            }
        } 
        #-------------------------
        # Getting correlations
        #-------------------------
        
        # Construct category list of factors
        if(!is.null(cat)) {
            catList <- list()
            for(i in cat){
                catList[[i]] <- dataframe[,i]
            }
        } else {
            catList <- rep(T, nrow(dataframe))
        }
        
        # Loop by categorie 
        results <- by(dataframe, catList, function(x, xVal, yVal, cat) { 
                      # Get correlations
                      corR <- cor.test(x[,xVal], x[,yVal])
                      corText <- paste0("r = ", signif(corR$estimate,labelRound) , "\np = ", signif(corR$p.value, labelRound))
                      result <- data.frame(r = corR$estimate, p =  corR$p.value, text = corText, stringsAsFactors = F)
                      # Append category names
                      if(!is.null(cat)) {
                          for(i in cat){
                              result <- cbind (result, x[1,i], stringsAsFactors = F)
                          }
                          colnames(result)[ (ncol(result) - length(cat) + 1) : ncol(result) ] <- cat
                      }
                      return(result)
                   }, xVal = x, yVal = y, cat = cat)
        
        results <- do.call(rbind, results)
        return(results)
        
}
