
# source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
# 
# Course ID:  rprog-014
# Submission Password:  6HaJmuP73f

rankhospital <- function(state, outcome, num="best") { 
     files <- list.files("rprog_data_ProgAssignment3-data", full.names=T)
     curfile <- files[3] #"outcome-of-care-measures.csv"
     #do not like num - not descriptive.  Changed to something I like
     record_num <- num
     
     # Invalid outcome input type 
     if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) { 
          stop("invalid outcome") 
     } 
     
     # Get index for our given outcome string.
     # http://www.inside-r.org/r-doc/base/ifelse
     # ifelse returns a value with the same shape as test which is filled with elements selected
     # from either yes or no depending on whether the element of test is TRUE or FALSE. 
     # ifelse(test, yes, no)
     # Below read it as if heart attack, assign col index = 11, if heart failure sel col= 17 otherwise set col = 23
     # Pretty sweet, huh?
     col_index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23)) 
     
     #Read and coerce our dataset while suppressing warnings and removing NA's. 
     data <- read.csv(curfile, colClasses="character") 
     # get all the rows using the col_index pasrse the data
     # do not care about coercion warnings.  coerce col_index character to numeric
     data[,col_index] <- suppressWarnings(as.numeric(data[,col_index])) 
     # Remove NAs this time
     data <- na.omit(data) 
     
     #Invalid state input or no observations 
     states <- table(data$State) 
     if (!state %in% names(states)) {  
          stop("invalid state") 
     } 
     
     #Slice our data by the given state and sort it by outcome and hospital name. 
     # Need to move away from subset.  See http://stackoverflow.com/questions/9860090/
     #Preferred method - even tho I like simplicity of 
     # subset (data_parsed <- subset(data, State==state))
     data_parsed <- data[data$State == state,]
     data_parsed <- data_parsed[order(data_parsed[,col_index], data_parsed[,2], na.last=TRUE),2] #this is ordered by rate 
     data_parsed <- na.omit(data_parsed) 
     
     # Really liking the ifelse.  Compact code.  Pretty easy to read.  Great solution for this where Best, Worse or a 
     #integral can be passed as athe functionns argument
     record_num <- ifelse(record_num == "best", 1, ifelse(record_num == "worst", length(data_parsed), as.numeric(record_num))) 
     
     #Get hospital name for the given rank by its 30-day mortality rate. 
     data_parsed[record_num] 
} 

# Sample Data
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
 