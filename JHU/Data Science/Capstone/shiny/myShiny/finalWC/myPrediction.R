suppressPackageStartupMessages(c(library(shiny), library(markdown),library(stylo))) #needed for cleanText function

# Load data
Data3Grams <- readRDS(file="./data/3gram.RData")
Data2Grams <- readRDS(file="./data/2gram.RData")
Data1Grams <- readRDS(file="./data/1gram.RData")

# a function from r-bloggers to clean invalid input in 'utf8towcs' - a few modifications
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
     
     toClean <- txt.to.words(toClean)
     return(toClean)
}

myPrediction <- function(wordCount,textInput)
{
     #how many words? 
     if (wordCount>=3) 
     {
          #want no more than 3 words (limits with ngrams bigger than 4)
          textInput <- textInput[(wordCount-2):wordCount] 
     }
     else if(wordCount==2) 
        {
          #if two words
          textInput <- c(NA,textInput)   
        }
     else 
        {
          #one word:
          textInput <- c(NA,NA,textInput)
        }
     #4gram:
     #predictedWord <- as.character(Data3Grams[Data3Grams$unigram==textInput[1] & Data3Grams$bigram==textInput[2] & Data3Grams$trigram==textInput[3],][1,]$quadgram)
     predictedWord <- as.character(Data3Grams[Data3Grams$unigram==textInput[1] & Data3Grams$bigram==textInput[2] & Data3Grams$trigram==textInput[3],][1,]$quadgram)
        
     #if failure
     if(is.na(predictedWord)) 
        {
          #3gram:
          predictedWord <- as.character(Data2Grams[Data2Grams$unigram==textInput[2] & Data2Grams$bigram==textInput[3],][1,]$trigram)

               #failure then
               if(is.na(predictedWord)) 
                {
                     #2gram
                     predictedWord <- as.character(Data1Grams[Data1Grams$unigram==textInput[3],][1,]$bigram)
                }
        }
      #Give a decent message if no words have been entered
      if(is.na(predictedWord))
        {
            predictedWord <- as.character("Oops! Please enter some words above.")
      }
     #predictedWord <- gsub("[^ a-zA-Z0-9<>#]+", "", predictedWord)#did not remove [ ]
     print(predictedWord)
}       