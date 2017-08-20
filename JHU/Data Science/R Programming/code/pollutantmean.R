pollutantmean <- function(directory,pollutant,id)  {
     
     browser()     
     
     #      Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or 
     #      nitrate) across a specified list of monitors. The function 'pollutantmean' takes three 
     #      arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers,
     #      'pollutantmean' reads that monitors' particulate matter data from the directory 
     #      specified in the 'directory' argument and returns the mean of the pollutant across 
     #      all of the monitors, ignoring any missing values coded as NA. A prototype of the function
     #      is as follows 
     
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
     ## NOTE: Do not round the result!
     
     #Not sure why this was necessry. Got rid of Error in base64(output) :  CHAR() can only be applied
     #to a 'CHARSXP', not a 'pairlist'
     alldatamerged <- c()
     files <- list.files(directory, full.names=T)
     requestedfiles <- files[id]#replace id
     #print(id)
     allrequesteddata <- lapply(requestedfiles,read.csv)#returns list of each file header=TRUE lapply always returns a list
     alldatamerged <- do.call(rbind,allrequesteddata)#combine all rows of data
     #write.table(alldatamerged, "Testing1.csv", sep = ",")
     
     # Get only the column of data needed
     pollutantdata <- alldatamerged[, pollutant]
     # Removal of NA values to be done after extracting "sulfate" column or "nitrate" col. If you remove NA 
     # values with all 4 columns, the answer for part 2 would be 1.732979 but if records having NA are removed 
     # only for "Sulfate" or " Nitrate" then result is 1.706
     pollutantdata <- na.omit(pollutantdata)
     # complete.cases does not work because ut results in TRUE FALSE
     # pollutantdata <- complete.cases(pollutantdata)
     #write.table(pollutantdata, "Nitrate.csv", sep = ",")
     return(mean(pollutantdata))#was print(mean . . .)
}

# source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
# pollutantmean("specdata", "sulfate", 1:10) == 4.064 
# pollutantmean("specdata", "nitrate", 70:72) == 1.706 
# pollutantmean("specdata", "nitrate", 23) == 1.281 

#Course ID: rprog-014
#User ID: 12305863 
#Submission Login: czweaver@hotmail.com 
#Submission Password: 6HaJmuP73f  


#Test data here:  https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcomplete-demo.html

