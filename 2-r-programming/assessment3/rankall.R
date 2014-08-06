rankall <- function(outcome, num = "best") {
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
    
    ## Check that outcome is valid
    validState <- sort(unique(data$State))
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    if(!is.element(outcome, validOutcome)) stop("invalid outcome")
    
    ## rename columns for easy reference
    names(data)[(3:5)] <- validOutcome
    
    ## order data by hospital name
    data <- data[order(data$Hospital.Name), ]
    
    ## helper function tu return hospital of a particular rank by state
    rankhospitalstate <- function(state) {
        ## subset data by state
        data <- subset(data, State == state)
        
        ## Return hospital name in that state with the given rank 30-day death rate
        if(is.numeric(num)) {
            ## if num is numeric, return the hospital in the num position from the sorted vector
            ## return NA if num is larger than the length of the sorted vector
            hospital.sorted <- data$Hospital.Name[order(data[, outcome])]
            if(num > length(hospital.sorted)) return(NA)
            return(hospital.sorted[num])
        } else {
            ## if num is "best" or "worst", return the best/worst hospital
            if(tolower(num) == "best") return(data$Hospital.Name[which.min(data[, outcome])])
            if(tolower(num) == "worst") return(data$Hospital.Name[which.max(data[, outcome])])
        }
    }
    
    ## For each state, find the hospital of the given rank
    hospital <- sapply(validState, rankhospitalstate)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(data.frame(hospital, state = validState))
}