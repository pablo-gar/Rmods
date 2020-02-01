# This a template for the use of optparse


suppressMessages(suppressWarnings({
    library("optparse") 
}))


main <- function() {
    
    opts <- getOpts(positional_arguments=F)
    input_file <- opts$i
    out_file <- opts$o
    
    # DO SOMETHING
    
}


getOpts <- function(positional_arguments=F) {
    
    # Specify the  type of options, this will aid for checking the type of opts
    required <- c('input', 'output') # Will  throw an error if these are not  given
    files <- c('input') # Will throw an error if the path to this options does not exist
    out_files <- c('output') # Will throw an error if the parent dir to these files don't exist
    dirs <- c() #  Will check that these folders exist
    
    # Create arg list
    option_list <- list(
                        make_option(c('-o', '--outoput'), type = 'character', help = 'Path to output file'),
                        make_option(c('-i', '--input'), type = 'character', help = 'Path to input file')
                        )
    
    # DO NOT MODIFY AFTER THIS
    opt_parser <-  OptionParser(usage='usage: %prog [options]', 
                                option_list=option_list, 
                                description='Calculate median gene expression across regions')
    
    opt <- parse_args(opt_parser, positional_arguments=positional_arguments)
    
    if(positional_arguments) {
        opt_check <- opt$options
    } else {
        opt_check <- opt
    }
    
    # Checking for essential arguments
    for (i in required) {
        if(is.null(opt_check[i][[1]])) {
            stop ('"--', i, '" is a required argument, run with "-h" for help')
        }
    }
    
    # Checking files exist
    for(i in files) {
        if(!file.exists(opt_check[i][[1]])) {
            stop ('"--', i, '" "', opt_check[i][[1]], '" file does not exist')
        }
    }
    
    # Checking that we can write out files
    for(i in out_files) {
        if(!dir.exists(dirname(opt_check[i][[1]]))) {
            stop ('"--', i, '" "', opt_check[i][[1]], '" parent folder does not exist')
        }
    }
    
    # Checking  dirs exists()
    for(i in dirs) {
        if(!dir.exists(opt_check[i][[1]])) {
            stop ('"--', i, '" "', opt_check[i][[1]], '" folder does not exist')
        }
    }
    
    return(opt)
    
}

main()
