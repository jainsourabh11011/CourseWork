# a function from r-bloggers to clean invalid input in 'utf8towcs' - slightly modified (last gsub)
cleanText <- function(toClean) {
     toClean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", toClean)
     toClean = gsub("@\\w+", "", toClean)
     toClean = gsub("[[:punct:]]", "", toClean)
     toClean = gsub("[[:digit:]]", "", toClean)
     toClean = gsub("http\\w+", "", toClean)
     #toClean = gsub("[ \t]{2,}", "", toClean)#Seems to scre up when a number is present
     toClean = gsub("^\\s+|\\s+$", "", toClean)
     toClean = gsub("amp", "", toClean)
     toClean = gsub("[^ a-zA-Z0-9<>#]+", " ", toClean)
     toClean = gsub("  "," ", toClean)#added 3-27-16 to remove double spaces between words
     # define "tolower error handling" function
     try.tolower = function(x) {
          y = NA
          try_error = tryCatch(tolower(x), error=function(e) e)
          if (!inherits(try_error, "error"))
               y = tolower(x)
          return(y)
     }
     
     toClean = sapply(toClean, try.tolower)
     toClean = toClean[toClean != ""]
     names(toClean) = NULL
     return(toClean)
}


