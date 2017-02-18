rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")

    ## Check that outcome is valid
    possibleOutcomes = c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, possibleOutcomes)) {
        stop("invalid outcome")
    }
    
    ## Determine data that corresponds to the outcome
    if (outcome == "heart attack") {
        indexval <- 11
    } else if (outcome == "heart failure") {
        indexval <- 17
    } else if (outcome == "pneumonia") {
        indexval <- 23
    } 
    
    ## Substitute row with %death rate of outcome to numeric values
    subrow <- gsub("Not Available", NA, outcome_data[, indexval])
    outcome_data[, indexval] <- as.numeric(subrow)
    

    ## Generate list with a data frame for each state
    listbystate <- split(outcome_data, outcome_data$State)
    
    results <- matrix(nrow = 54, ncol = 2)

    for (i in 1:length(listbystate)) {

        state_data <- listbystate[[i]]

        ## Re-order the state_data
        ### first by hospital name in ascending order
        ### then by death rate outcome in ascending order
        orderbyname <- state_data[order(state_data$Hospital.Name),]
        ordereddata <- orderbyname[order(orderbyname[,indexval],
                                              na.last = NA),]

        curstate <- ordereddata$State[[1]]


        if (is.numeric(num) == TRUE) {
            newrow <- c(ordereddata$Hospital.Name[num], curstate)
        } else if (num == "best") {
            newrow <- c(ordereddata$Hospital.Name[[1]], curstate)
        } else if (num == "worst") {
            newrow <- c(ordereddata$Hospital.Name[dim(ordereddata)[1]], curstate)
        }

        results[i,] <- newrow

    }


    ## For each state, find the hospital of the given rank


    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    rnames <- results[,2]
    df <- data.frame(results)
    names(df) <- c("hospital", "state")
    row.names(df) <- rnames
    
    df

}