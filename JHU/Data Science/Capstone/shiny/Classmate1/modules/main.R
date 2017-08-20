library(quanteda)
library(digest)
library(quanteda)

# Load the ngrams and related counts
# source("load_data.R")
rm(list=ls())
load(file="data/ngrams.RData")
load(file="data/freq.RData")

# Load module for word prediction functions
source("modules/word_predict.R")




