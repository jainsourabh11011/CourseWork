# Create mono,bi and trigrams file from a corpora text file
#
# If required the orginal source file must be converted to ascii, in order to remove
# "dirty" characters.
#
# i.e. iconv -c -f utf8 -t ascii corpora.txt > corpora_ascii.txt
#
# Author: filqua74



library(quanteda)
library(digest)

outputGram <- function(x) {
  phrase <- x
  phrase <- gsub(",",".",phrase)
  # Verify if single word
  tmp <- strsplit(phrase, split = " ")[[1]]
  nword <- length(tmp)
  phrase_1 <- paste(as.list(tmp[1:nword]),collapse=",")
  output <- paste(phrase_1, "\n", sep="")  
  return(output)
}

generateNgramsFile <- function(filename,size,sample, suff="") {
  con <- file(paste(filename,".txt",sep = "")) 
  fileoutname <- paste(filename,".",suff,as.character(size),"grams.txt", sep="")
  print(fileoutname)
  sink(fileoutname)
  open(con)
  while (length(line <- readLines(con, n = 1, encoding = "UTF-8", warn = FALSE)) > 0) {
    flip <- rbinom(1,1,sample)
    if (flip==1) {
      tokens <- tokenize(toLower(line),removeNumbers = TRUE, removePunct = TRUE, removeTwitter=TRUE)
      #tokens <- removeFeatures(tokens, stopwords("english"))
      res <- ngrams(tokens, n = size, concatenator = " " )[[1]]
      sapply(res, function(x) cat(outputGram(x)))
    }
  }
  sink()
  close(con)
}

generateNgramsFile("corpora/corpora_ascii",1,1/3)
generateNgramsFile("corpora/corpora_ascii",2,1/2)
generateNgramsFile("corpora/corpora_ascii",3,1/2)
# As testing set
generateNgramsFile("corpora/corpora_ascii",3,1/5,suff="test_")

