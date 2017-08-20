
#Back Off Algorithm

# Predict the next word from a group or words as a user enters it
# Begin with 4Gram; use thelast three words the user provides
# If no 4Gram is found back off to 3Gram 
# If no 3Gram is found back off to 2Gram 
# If no 2Gram is found back off to 1Gram and use the most common word
#
library(stringr)
#source("./codeWIP/cleaning.R")Turned off - already in server.R
# nGram1 <- as.data.frame(read.csv("./Capstone/shiny/myShiny/data/nGram1.csv"))
# nGram2 <- as.data.frame(read.csv("./Capstone/shiny/myShiny/data/nGram2.csv"))
# nGram3 <- as.data.frame(read.csv("./Capstone/shiny/myShiny/data/nGram3.csv"))
# nGram4 <- as.data.frame(read.csv("./Capstone/shiny/myShiny/data/nGram4.csv"))

PredictWord <- function(inputString)
{
     #assign("result", "in PredNextTerm", envir = .GlobalEnv)#original

     # Clean the input string
     inputString <- cleanText(inputString)
     
     # Split the string and get length
     inputString <- unlist(strsplit(inputString, split=" "))
     inputString_Length <- length(inputString)
     
     wordFOUND <- FALSE
     predictedWord <- as.character(NULL)
     search_result <- NULL
     result <- NULL
     
     #First test the 4Gram
     if (inputString_Length >= 3 & !wordFOUND)
     {
          #keep last 3 words
          tmpTestString <- paste(inputString[(inputString_Length-2):inputString_Length], collapse=" ")
          # Subset 4Gram; use regex
          search_string <- paste("^",tmpTestString, sep = "")
          search_result <- nGram4[grep(search_string, nGram4$terms), ]
          # Check if a match was found
          if(nrow(search_result) >0)
          {
               predictedWord <- search_result[1,1]
               wordFOUND <- TRUE
               result <- "The predicted word was found using a 4 n-gram"
          }
     }
     if(inputString_Length >= 2 & !wordFOUND)
     {
          # Assemble the terms of the input string separated by one white space each
          tmpTestString <- paste(inputString[(inputString_Length-1):inputString_Length], collapse=" ")#keep last 2 words
          # Subset the Three Gram data frame 
          search_string <- paste("^",tmpTestString, sep = "")
          search_result <- nGram3[grep(search_string, nGram3$terms), ]
          # Check to see if any matching record returned
          if(nrow(search_result) >0)
          {
               predictedWord <- search_result[1,1]
               wordFOUND <- TRUE
               result <<- "The predicted word was found using a 3 n-gram:"
          }
     }
     if (inputString_Length >= 1 & !wordFOUND)
     {
          tmpTestString <- inputString[inputString_Length]
          search_string <- paste("^",tmpTestString, sep = "")
          search_result <- nGram2[grep(search_string, nGram2$terms), ]
          
          if(nrow(search_result) >0)
          {
               predictedWord <- search_result[1,1]
               wordFOUND <- TRUE
               result <- "The predicted word was found using a 2 n-gram:"
          }
     }
     #Use most frequent word from the 1Gram
     if (!wordFOUND & inputString_Length > 0)
     {
          predictedWord <- nGram1$terms[1]
          result <- "The predicted word has been identified by a simple frequency table:"
     }
     nextTerm <- word(predictedWord, -1)

     if (inputString_Length > 0){
          result_message <- data.frame(result, nextTerm)
          return(result_message)
     } else {
          nextTerm <- ""
          result <-"Hmm, you might have found a bug.  Please try again."
          result_message <- data.frame(result, nextTerm)
          return(result_message)
     }
}

##############Prediction 2######################

# suppressPackageStartupMessages(c(
#      library(shinythemes),
#      library(shiny),
#      library(tm),
#      library(stringr),
#      library(markdown),
     library(stylo)

# final4Data <- readRDS(file="./data/final4Data.RData")
# final3Data <- readRDS(file="./data/final3Data.RData")
# final2Data <- readRDS(file="./data/final2Data.RData")

source("../codeWIP/cleaning.R")

cleanInput <- function(text){
     textInput <- cleanText(text)
     return(unlist(stri_split(textInput, regex = "[[:space:]]+", omit_empty = T)))
}

nextWordPrediction <- function(wordCount,textInput){
     if (wordCount>=3) {
          textInput <- textInput[(wordCount-2):wordCount] 
     }
     else if(wordCount==2) {
          textInput <- c(NA,textInput)   
     }
     else {
          textInput <- c(NA,NA,textInput)
     }
     wordPrediction <- as.character(final4Data[final4Data$unigram==textInput[1] & 
                                                    final4Data$bigram==textInput[2] & 
                                                    final4Data$trigram==textInput[3],][1,]$quadgram)
     if(is.na(wordPrediction)) {
          wordPrediction1 <- as.character(final3Data[final3Data$unigram==textInput[2] & 
                                                          final3Data$bigram==textInput[3],][1,]$trigram)
          if(is.na(wordPrediction)) {
               wordPrediction <- as.character(final2Data[final2Data$unigram==textInput[3],][1,]$bigram)
          }
     }
     print(wordPrediction)
}
