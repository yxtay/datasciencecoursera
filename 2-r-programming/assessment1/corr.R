corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    cases <- complete(directory)
    
    id <- cases$id[cases$nobs > threshold]
    if (length(id) > 0) {
        file <- paste(directory, sprintf("%03d.csv", id), sep = "/")
        data <- lapply(file, read.csv)
        cr <- sapply(data, function(df) cor(df$sulfate, df$nitrate, use = "pairwise.complete.obs"))
    } else {
        numeric(0)
    }
}