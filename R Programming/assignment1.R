setwd("C:/Users/YuXuan/Documents/Git/datasciencecoursera/R Programming")

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    file <- paste(directory, sprintf("%03d.csv", id), sep = "/")
    
    data <- lapply(file, read.csv)
    data <- do.call(rbind, data)
    
    return(mean(data[, pollutant], na.rm = T))
}

source("pollutantmean.R")

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    file <- paste(directory, sprintf("%03d.csv", id), sep = "/")
    data <- lapply(file, read.csv)
    nobs <- sapply(data, function(df) sum(complete.cases(df)))
    
    return(data.frame(id, nobs))
}

source("complete.R")

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

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

source("corr.R")
source("complete.R")

cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
length(cr)
