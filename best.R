best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
    ## Check that state is valid
    if (!is.element(state, outcome_data$State)) {
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    possibleOutcomes = c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, possibleOutcomes)) {
        stop("invalid outcome")
    }


    library(plyr)

    if (outcome == "heart attack") {
        indexval <- 11
    } else if (outcome == "heart failure") {
        indexval <- 17
    } else if (outcome == "pneumonia") {
        indexval <- 23
    }
    
    #Generate list with a data frame for each state
    listbystate <- split(outcome_data, outcome_data$State)
    
    # Generate data frame for state of interest only
    state_data <- listbystate[[state]]
    
    # Arrange data in alphabetical order by hospital name
    arranged_data <- arrange(state_data, state_data$Hospital.Name)
    
    # Substitute "Not Available" in outcome row to NA to avoid coercion errors
    outcomerow <- gsub("Not Available", NA, arranged_data[, indexval])
    
    rownum <- which.min(outcomerow) #index of first row with lowest death rate
    BestHospital <- arranged_data$Hospital.Name[rownum]

    # outcome_data2 <- as.numeric(outcome_data[, indexval])
    # ## The above causes coercion warning message because "Not Available" 
    #    is coerced to NA
    # 
    # arranged_data <- arrange(outcome_data, outcome_data$State, 
    #                          outcome_data2, outcome_data$Hospital.Name)
    #     listbystate <- split(arranged_data, arranged_data$State)
    #     StateList <- listbystate[[state]]
    #     BestHospital <- StateList$Hospital.Name[[1]]
        
    BestHospital
}