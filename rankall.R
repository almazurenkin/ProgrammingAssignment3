rankall <- function(outcome, num = "best") {
        ## Initialization
        mapping <- c(11, 17, 23)
        names(mapping) <- c("heart attack", "heart failure", "pneumonia")
        result <- data.frame()
        
        ## Check that state and outcome are valid
        if (is.na(mapping[outcome])) stop("invalid outcome") 
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Processing
        data <- data[, c(2, 7, mapping[outcome])] # Reduce data frame to only relevant columns
        names(data) <- c("hospital", "state", "rate") # Assign shorter names for convenience
        data$rate <- as.numeric(data$rate) # Coerce rates from character to numeric
        
        data <- split(data, data$state) # Split data set per state
        
        for(buffer in data) {

                ## Sort
                buffer <- buffer[order(buffer$rate, buffer$hospital, na.last = NA), ]
        
                ## Processing 'num' agrument that can be "best", "worst", or numeric
                i <- if (num == "best") 1 else if (num == "worst") nrow(buffer) else num

                ## Add to result data frame hospital in that state with the given rank 30-day death rate
                result <- rbind(result, data.frame(hospital = buffer$hospital[i], state = buffer$state[1]))  
        }
        
        row.names(result) <- result$state
        return(result) 
}