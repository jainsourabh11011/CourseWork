# Load ngrams in memory, perform main counts and create binary images on file
#
# Author: filqua74

library(dplyr)
library(data.table)

monograms <- as.data.table(read.csv("corpora/corpora_ascii.1grams.txt",header = F, stringsAsFactors = F,colClasses = c(rep("character",1))))
monograms <- monograms %>% group_by(V1) %>% summarize(count=n()) %>% ungroup %>% arrange(desc(count))
monfreq <- monograms %>% group_by(count) %>% summarize(freq=n())  %>%
           select(count,freq) %>% arrange(count) %>% ungroup()
monograms <- head(monograms, 50000)

bigrams <- as.data.table(read.csv("corpora/corpora_ascii.2grams.txt", header = F, stringsAsFactors = F, colClasses = c(rep("character",2))))
bigrams <- bigrams %>% group_by(V1,V2) %>% summarize(count=n()) %>% ungroup %>% arrange(desc(count))
bigfreq <- bigrams %>% group_by(count) %>% summarize(freq=n()) %>% 
           select(count,freq) %>% arrange(count) %>% ungroup()
bigrams <- head(bigrams,1500000)

trigrams <- as.data.table(read.csv("corpora/corpora_ascii.3grams.txt", header = F, stringsAsFactors = F, colClasses = c(rep("character",3))))
trigrams <- trigrams %>% group_by(V1,V2,V3) %>% summarize(count=n()) %>% ungroup %>% arrange(desc(count))
trifreq <- trigrams %>% group_by(count) %>% summarize(freq=n()) %>% 
           select(count,freq) %>% arrange(count) %>% ungroup()
trigrams <- head(trigrams,1500000)

save(monograms,bigrams,trigrams,file="data/ngrams.RData")
save(monfreq,bigfreq,trifreq,file="data/freq.RData")

