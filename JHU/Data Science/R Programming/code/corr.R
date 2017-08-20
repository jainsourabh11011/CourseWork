#    Write a function that takes a directory of data files and a threshold for complete cases and 
#    calculates the correlation between sulfate and nitrate for monitor locations where the number of 
#    completely observed cases (on all variables) is greater than the threshold. The function should 
#    return a vector of correlations for the monitors that meet the threshold requirement. If no 
#    monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 
#    A prototype of this function follows        

corr <- function(directory, threshold = 0) { 

     browser()
     
     #files <- list.files( path = directory ) 
     # Could have done it the other way:
     files <- list.files(directory, full.names=T)
     
     cr <- c()  
        
     for(f in 1:length(files)){ 
          #3data <- read.csv( paste(directory, "/", files[f], sep="") ) 
          # Could have done this the other way:
          data <- read.csv(files[f])
          data <- data[complete.cases(data),] 
          if ( nrow(data) > threshold ) { 
               #Add tot eh cr vector the cor value derived from the sulfate and nitrate colums in each file
               cr <- c(cr, cor(data$sulfate, data$nitrate) ) # append correlations 
                    } 
             } 
         
        return( cr ) 
      }
     #var, cov and cor compute the variance of x and the covariance or correlation of x and y if these are vectors. 
     #If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y 
     #are computed.

#Examples founf here: https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html
# cr <- corr("specdata", 150)
# cr <- corr("specdata", 400)
# cr <- corr("specdata")
# cr <- corr("specdata", 5000)