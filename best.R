best <- function(state, outcome) {
    columnId <- NA
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states <- unique(data$State)
    if(!state %in% states ) stop("invalid state")
    
    ## while checking for valid outcome, also store column for valid outcomes
    if(outcome == "heart attack" ){
        columnId <- 11
    }else if (outcome == "heart failure"){
        columnId <- 17
    }else if (outcome == "pneumonia"){
        columnId <- 23
    }else {
        stop("invalid outcome")
    }
    
    ## subset data by state, keep only columns with hospital name and 
    ## the data for the specified outcome
    dataSub <- subset(data, data$State == state, c(2,columnId))
    
    ## remove rows that have "Not Available" in the data column
    avail <- dataSub[, 2] != "Not Available"
    dataSub <- dataSub[avail, ]
    
    ## convert data from character to numeric
    dataSub[,2] <- as.double(dataSub[,2])
    
    ## find the Hospitals with the mininum value
    minVal <- min(dataSub[,2], na.rm = TRUE)
    names <- subset(dataSub, dataSub[, 2] == minVal, 1)
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## if more than one match return the first alphabetically
    min(names$Hospital.Name)

    
}