best <- function(state, outcome) {
        ## Initialization: create index mapping vector with named values
        mapping <- c(11, 17, 23)
        names(mapping) <- c("heart attack", "heart failure", "pneumonia")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!state %in% data$State) stop("invalid state")
        if (is.na(mapping[outcome])) stop("invalid outcome")
         
        ## Processing
        buffer <- data[, c(2, 7, mapping[outcome])] # Reduce data frame to only relevant columns
        names(buffer) <- c("hospital", "state", "rate") # Assign shorter names for convenience
        buffer$rate <- as.numeric(buffer$rate) # Coerce rates from character to numeric
        buffer <- buffer[buffer$state == state & !is.na(buffer$rate), ] # Filter rows in needed state and remove rows with NA rate
        
        ## Sort
        buffer <- buffer[order(buffer$rate, buffer$hospital), ]
        
        ## Return hospital name in that state with lowest 30-day death rate
        return(buffer$hospital[1])        
}