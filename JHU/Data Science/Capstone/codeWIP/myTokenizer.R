options(java.parameters = "-Xmx4g")

# Define file names with relative folder path
#setwd("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone")#work
setwd("C:/Users/czwea/Documents/GitHub/Coursera_DataScientist/Capstone")#Home

library(tm) # Framework for text mining.
#library(SnowballC) # Provides wordStem() for stemming.
#library(qdap) # Quantitative discourse analysis of transcripts.
#library(qdapDictionaries)
library(dplyr) # Data preparation and pipes %>%.
#library(RColorBrewer) # Generate palette of colours for plots.
#library(ggplot2) # Plot word frequencies.
#library(scales) # Include commas in numbers.
#library(RCurl)
#library(XML)
#library(reshape2)
library(RWeka)
library(data.table)
library(stringi)
source("./codeWIP/cleaning.R")

#Load in raw data sets
# n The (maximal) number of lines to read. Negative values indicate that one should read up to the end of input on the connection.
originalBlogs <- readLines("./data/final/en_US/en_US.blogs.txt",encoding="UTF-8", n=-1L)
originalNews <- readLines("./data/final/en_US/en_US.news.txt",encoding="UTF-8", n=-1L, warn = FALSE)
originalTwitter <- readLines("./data/final/en_US/en_US.twitter.txt",encoding="UTF-8", skipNul = TRUE, n=-1L)
#remove banned words from URL below
#https://gist.github.com/jamiew/1112488
badwords <- readLines("./data/badWords.txt", encoding="UTF-8")

sampleBlogs <- sample (originalBlogs, 500000, replace=FALSE)#was 20000
sampleNews <- sample (originalNews, 100000, replace=FALSE)#was 20000
sampleTwitter <- sample (originalTwitter, 2000, replace=FALSE)#was 5000

sampleText <- c(sampleBlogs, sampleNews, sampleTwitter)

sampleText <- cleanText(sampleText)

rm(originalBlogs, originalNews, originalTwitter, sampleBlogs, sampleNews, sampleTwitter)

# Divide data into small chunks
noOfRecords <- length(sampleText)
maxRecordstoProcess = 2000#was 2000
loopNumber <- noOfRecords%/%maxRecordstoProcess

# Create term document matrix
ngram1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
ngram2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
ngram3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
ngram4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

loopNgram1 <- "./data/tmpNgram1_"
loopNgram2 <- "./data/tmpNgram2_"
loopNgram3 <- "./data/tmpNgram3_"
loopNgram4 <- "./data/tmpNgram4_"

countNoRecords <-0
for (i in 1:loopNumber ) {
     recordCountStart <- (i-1)*maxRecordstoProcess+1
     recordCountEnd <- i*maxRecordstoProcess
     tmpSampleSubset <- sampleText[recordCountStart:recordCountEnd]
     # Create Corpus
     tmpCorpus <- VCorpus(VectorSource(tmpSampleSubset))
     
     tmpCorpus <- tm_map(tmpCorpus, removeWords, badwords)

     tdm1 <- TermDocumentMatrix(tmpCorpus, control = list(tokenize = ngram1))
     tdm2 <- TermDocumentMatrix(tmpCorpus, control = list(tokenize = ngram2))
     tdm3 <- TermDocumentMatrix(tmpCorpus, control = list(tokenize = ngram3))
     tdm4 <- TermDocumentMatrix(tmpCorpus, control = list(tokenize = ngram4))
     
     freq1 <- sort(rowSums(as.matrix(tdm1)), decreasing=TRUE)#frequency table
     freq2 <- sort(rowSums(as.matrix(tdm2)), decreasing=TRUE)
     freq3 <- sort(rowSums(as.matrix(tdm3)), decreasing=TRUE)
     freq4 <- sort(rowSums(as.matrix(tdm4)), decreasing=TRUE)
     
     DT1 <- data.table(terms=names(freq1), freq=freq1)#make into data table
     DT2 <- data.table(terms=names(freq2), freq=freq2)
     DT3 <- data.table(terms=names(freq3), freq=freq3)
     DT4 <- data.table(terms=names(freq4), freq=freq4)
     
     tmpNgram1 <- paste(loopNgram1,i,sep="")#save temp file
     tmpNgram2 <- paste(loopNgram2,i,sep="")
     tmpNgram3 <- paste(loopNgram3,i,sep="")
     tmpNgram4 <- paste(loopNgram4,i,sep="")

     write.table(DT1, tmpNgram1, col.names = TRUE, row.names = FALSE)#write temp file
     countNoRecords = countNoRecords+1
     write.table(DT2, tmpNgram2, col.names = TRUE, row.names = FALSE)
     countNoRecords = countNoRecords+1
     write.table(DT3, tmpNgram3, col.names = TRUE, row.names = FALSE)
     countNoRecords = countNoRecords+1
     write.table(DT4, tmpNgram4, col.names = TRUE, row.names = FALSE)
     countNoRecords = countNoRecords+1
     
     print(paste("Loop:", i,"of", loopNumber, "subset", recordCountStart, "-", recordCountEnd, ";",
                 countNoRecords, "files created", sep=" "))
}


#Collaspse all the individual files
#Test these##################
#
filenames1 <- list.files(path = "./data/", pattern = "tmpNgram1_*")
filenames2 <- list.files(path = "./data/", pattern = "tmpNgram2_*")
filenames3 <- list.files(path = "./data/", pattern = "tmpNgram3_*")
filenames4 <- list.files(path = "./data/", pattern = "tmpNgram4_*")

fileSummed_ngram1 <-do.call("rbind", lapply(paste("./data/", filenames1, sep=""), read.table, header = TRUE))
fileSummed_ngram2 <-do.call("rbind", lapply(paste("./data/", filenames2, sep=""), read.table, header = TRUE))
fileSummed_ngram3 <-do.call("rbind", lapply(paste("./data/", filenames3, sep=""), read.table, header = TRUE))
fileSummed_ngram4 <-do.call("rbind", lapply(paste("./data/", filenames4, sep=""), read.table, header = TRUE))

#Ensuring the grouping and sorting was right took much time.  So much data, you really need to l0ook at everything.
ngram1 <- data.table(fileSummed_ngram1)#use data.table
ngram1 <- ngram1[, sum(freq), by=terms]#group
ngram1 <- ngram1[order(-V1)]#sort

ngram2 <- data.table(fileSummed_ngram2)#use data.table
ngram2 <- ngram2[, sum(freq), by=terms]#group
ngram2 <- ngram2[order(-V1)]#sort

ngram3 <- data.table(fileSummed_ngram3)#use data.table
ngram3 <- ngram3[, sum(freq), by=terms]#group
ngram3 <- ngram3[order(-V1)]#sort

ngram4 <- data.table(fileSummed_ngram4)#use data.table
ngram4 <- ngram4[, sum(freq), by=terms]#group
ngram4 <- ngram4[order(-V1)]#sort

# Subset the data frames based on the frequency - change to use dplyr with statements above
ngram1 <- ngram1[ngram1$V1 > .,]#was 20
ngram2 <- ngram2[ngram2$V1 > 1,]#was 2
ngram3 <- ngram3[ngram3$V1 > 1,]#was 2
ngram4 <- ngram4[ngram4$V1 > 1,]

#Save the nGrams
write.csv(ngram1, "./data/nGram1.csv", row.names = FALSE)
write.csv(ngram2, "./data/nGram2.csv", row.names = FALSE)
write.csv(ngram3, "./data/nGram3.csv", row.names = FALSE)
write.csv(ngram4, "./data/nGram4.csv", row.names = FALSE)

rm(list= ls()[!(ls() %in% c("nGram1.txt","nGram2.txt", "nGram3.txt", "nGram4.txt"))])
 
setwd("./data")
filez <- list.files(pattern="tmpNgram")
file.remove(filez)


