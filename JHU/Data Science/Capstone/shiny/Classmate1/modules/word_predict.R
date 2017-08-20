# Provides the logic for word prediction
#
# Author: filqua74

library(digest)
library(quanteda)
library(dplyr)

# Load module for good-turing smoothing
source("modules/goodturing.R")

#wordcount <- function(word) {
#    tmp <- monograms[monograms$V1==word,]
#    if (length(tmp)==0) {
#      res = 0
#    } else {
#      res <- tmp$count
#    }
#    return(res)
#}


candidates_list <- function(tokens,n) {
  ntoks <- length(tokens)
  if (ntoks==2) {
    tmp <- filter(trigrams, V1==tokens[ntoks-1] & V2==tokens[ntoks])
    if (nrow(tmp)!=0) {
      return(head(tmp$V3,n))
    }
    ntoks=1
  } 
  
  if (ntoks==1) {
    tmp <- filter(bigrams, V1==tokens[ntoks]) %>%
           select(V2)
    if (nrow(tmp)!=0) {
      return(head(tmp$V2,n))
    }
  }
  
  tmp <- head(monograms,n)
  return(head(tmp$V1,n))
}

estimateProb <- function(word,tokens) {
  #word <- wordstem(word)
  ntoks <- length(tokens)  
  if (ntoks==2) {
    tmp3 <- filter(trigrams, V1==tokens[ntoks-1] & V2==tokens[ntoks] & V3==word)$count
    if (length(tmp3)!=0) {
      p3 <- estProbTri(tmp3)
      return(p3)
    } 
    ntoks = 1
  }
  if (ntoks==1) {
    tmp2 <- filter(bigrams, V1==tokens[ntoks] & V2==word)$count
    if (length(tmp2)!=0) {
      p2 <- estProbTri(0)*estProbBig(tmp2)
      return(p2)
    } 
  }

  tmp1 <- filter(monograms, V1==word)$count
  if (length(tmp1)==0) {
    p1 <- estProbMon(0)
  } else {
    p1 <- estProbTri(0)*estProbBig(0)*estProbMon(tmp1)
  }
  
  return(p1)
}

wordPredict <- function(phrase,n=2,ncand=10) {
  tokens <- tokenize(toLower(phrase),removeNumbers = TRUE, removePunct = TRUE, removeTwitter=TRUE)[[1]]
  #tokens <- removeFeatures(tokens, stopwords("english"))[[1]]
  ntoks <- length(tokens) 
  tokens <- tokens[(ntoks-n+1):ntoks] # last n-gram considered
  # Find candidates
  cands <- candidates_list(tokens,ncand)
  score <- sapply(cands,estimateProb,tokens)
  score <-array(unlist(score), dim = c(dim(score[[1]]), length(score)))
  res <- data.frame(word=cands,score=score,stringsAsFactors = FALSE)
  rownames(res) <- NULL
  return(res[order(res$score,decreasing = TRUE),]) 
}