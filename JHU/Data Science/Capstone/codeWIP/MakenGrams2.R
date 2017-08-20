
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data preparation and pipes %>%.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(filehash)
library(RCurl)
library(XML)
library(reshape2)
library(RWeka)

# Define file names with relative folder path
setwd("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone")#work
setwd("C:/Users/czwea/Documents/GitHub/Coursera_DataScientist/Capstone")#Home

#Load in raw data sets
blogs.Raw <- readLines("./data/final/en_US/en_US.blogs.txt",encoding="UTF-8")
news.Raw <- readLines("./data/final/en_US/en_US.news.txt",encoding="UTF-8")
twitter.Raw <- readLines("./data/final/en_US/en_US.twitter.txt",encoding="UTF-8", skipNul = TRUE)
#remove banned words from URL below
#https://gist.github.com/jamiew/1112488
profanity <- readLines("banned_words.txt",encoding="UTF-8")


#find the length of each item in each character vector
# blogs.Length<-unlist(lapply(blogs.Raw,nchar),recursive=TRUE)
# news.Length<-unlist(lapply(news.Raw,nchar),recursive=TRUE)
# twitter.Length<-unlist(lapply(twitter.Raw,nchar),recursive=TRUE)
# 
# max_lengths <- c(max(blogs.Length),max(news.Length),max(twitter.Length))
# standard_dev <- c(sd(blogs.Length),sd(news.Length),sd(twitter.Length))
# number_of_entries <- c(length(blogs.Raw), length(news.Raw), length(twitter.Raw))
# 
# summary_res<- data.frame(number_of_entries,max_lengths, standard_dev)
# rownames(summary_res)<-c("blogs","news","twitter")
# 
# 
# summary_res

#Samples are drawn from each for faster processing

blogs.sample <- sample (blogs.Raw,1000, replace=FALSE)
news.sample <- sample (news.Raw,1000, replace=FALSE)
twitter.sample <- sample (twitter.Raw,500, replace=FALSE)

sample.Raw <- c(blogs.sample, news.sample, twitter.sample)

source("./codeWIP/cleaning.R")

sample.Raw.Clean <- clean.text(sample.Raw)


remove(blogs.Raw)
remove(twitter.Raw)
remove(news.Raw)
remove(blogs.sample)
remove(news.sample)
remove(twitter.sample)
remove(blogs.Length)
remove(news.Length)
remove(twitter.Length)
remove(standard_dev)
remove(number_of_entries)
remove(max_lengths)

remove(summary_res)


#full.Corpus <- data.frame(lapply(sample.Raw, type.convert), stringsAsFactors=FALSE)
#full.Corpus<- do.call("rbind", lapply(full.Corpus, as.data.frame))
#full.Corpus <- Corpus(as.data.frame(t(sample.Raw)))



full.Corpus <- Corpus(VectorSource(sample.Raw.Clean))
remove(sample.Raw)


#Process the sampled text for easier processing

#only alphanumeric characters
full.Corpus <- tm_map(full.Corpus, function(x) gsub('[^[:alnum:] ]', "", x))
# remove numbers
full.Corpus <- tm_map(full.Corpus, removeNumbers)
#all uppercase become lowercase
full.Corpus <- tm_map(full.Corpus, tolower)
# remove stopwords
# full.Corpus <- tm_map(full.Corpus, removeWords, stopwords('english'))
#white space
full.Corpus <- tm_map(full.Corpus, stripWhitespace)


full.Corpus <- tm_map(full.Corpus, removeWords, profanity)

#remove special characters
full.Corpus <- tm_map(full.Corpus, function(x) gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", x)) 


#read into single text document
remove(profanity)

textdoc <- gsub("http\\w+", "", full.Corpus)[1]
remove(full.Corpus)
#textdoc <- data.frame(text=unlist(sapply(full.Corpus, `[`, "content")), stringsAsFactors=F)

#turn it back into a text doc(thanks 'tolower'...)
#full.Corpus <- tm_map(full.Corpus, PlainTextDocument)

#stem
#full.Corpus <- tm_map(full.Corpus, stemDocument)

#removed banned words from URL below
#https://gist.github.com/jamiew/1112488



onegram <- NGramTokenizer(textdoc, Weka_control(min = 1, max = 1))
twogram <- NGramTokenizer(textdoc, Weka_control(min = 2, max = 2))
threegram <- NGramTokenizer(textdoc, Weka_control(min = 3, max = 3))



oneg <- data.frame(table(onegram))
twog <- data.frame(table(twogram))
thrg <- data.frame(table(threegram))

remove(onegram)
remove(twogram)
remove(threegram)


onesorted <- oneg[order(oneg$Freq,decreasing = TRUE),]
twosorted <- twog[order(twog$Freq,decreasing = TRUE),]
thrsorted <- thrg[order(thrg$Freq,decreasing = TRUE),]

onetop <- onesorted[1:10,]
twotop <- twosorted[1:10,]
thrtop <- thrsorted[1:10,]



# par(las=2)
# barplot(onetop$Freq[1:10],
#         main="Frequency of Most Common Single Words",
#         names.arg=onetop$onegram[1:10])
# 
# par(las=2)
# barplot(twotop$Freq[1:10],
#         main="Frequency of Most Common Two-Word Pairs",
#         names.arg=twotop$twogram[1:10])
# 
# par(las=2)
# barplot(thrtop$Freq[1:10],
#         main="Frequency of Most Common Three-Word Sets",
#         names.arg=thrtop$threegram[1:10])



