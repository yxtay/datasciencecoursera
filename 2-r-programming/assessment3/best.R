best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     na.strings = "Not Available", stringsAsFactors = F)
    
    ## subset data frame to only include relevant columns
    colKeep <- grep("hospital.name|state", names(data), ignore.case = T)
    ## columns of interest have the following form:
    ## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    ## use of regex to locate them
    colKeep <- c(colKeep, grep("^hospital(.*)death(.*)(heart.attack|heart.failure|pneumonia)",
                               names(data), ignore.case = T))
    data <- subset(data, select = colKeep)
    
    ## Check that state and outcome are valid
    validState <- sort(unique(data$State))
    if(!is.element(state, validState)) stop("invalid state")
    
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    if(!is.element(outcome, validOutcome)) stop("invalid outcome")
    
    ## rename columns for easy reference
    names(data)[(3:5)] <- validOutcome
    
    ## subset data by state and order by name
    data <- subset(data, State == state)
    data <- data[order(data$Hospital.Name), ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    return(data$Hospital.Name[which.min(data[, outcome])])
}