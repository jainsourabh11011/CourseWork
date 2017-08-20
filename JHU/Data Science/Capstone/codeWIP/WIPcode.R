
########### MY CODE ####################

##Download the Data


#library(downloader)
# if(!file.exists("../Capstone/data/rawData.zip")){
#      fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#      download(fileURL, dest = "../Capstone/data/rawData.zip")
#      unzip("../Capstone/data/rawData.zip", exdir="../Capstone/data")#folders:  data/final/en_US
# }

#Would never had known how to do this without this entry in the course discussions
##https://www.coursera.org/learn/data-science project/discussions/WB6O7eb9EeWhmg4JHc-OZw?sort=createdAtDesc&page=1
##Unable to use relative path with file
conBlogs <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.blogs.txt", "rb")
englishBlogs <- readLines(conBlogs)
close.connection(conBlogs)
rm(conBlogs)

conNews <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.news.txt", "rb")
englishNews <- readLines(conNews)
close.connection(conNews)
rm(conNews)

conTwitter <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.twitter.txt", "rb")
englishTwitter <- readLines(conTwitter, skipNul = TRUE)
close.connection(conTwitter)#Prevents warnings like In readLines(conTwitter):line 1759032 appears to contain an embedded nul
rm(conTwitter)

#Create an example file to include in body of report.  Select text that has good examples in it
#qdap tranformations will be done first so change to data frame
sampleTwitterDF <- as.data.frame(englishTwitter[1:600])#Used for before and after example
sampleTwitterDF <- as.character(sampleTwitterDF[c(3:5,20, 64, 72, 76:77, 172, 215, 572),1])

sampleTwitterDF_AFTER <- sampleTwitterDF
#showConnections()
#closeAllConnections()


load("~/Github/Coursera_DataScientist/Capstone/data/data_Summary.Rda")
#exists("capstone1.Rda")#Hoperfully returns true!
datatable(data_summary, caption = "Data File Information - Before Processing", options = list(searching=FALSE, paging=FALSE))

library(tm)
library(qdap)
library(qdapRegex)
#qdapRegex is a collection of regular expression tools associated with the qdap package that may be useful outside of the context of discourse analysis. Tools include removal/extraction/replacement of abbreviations, dates, dollar amounts, email addresses, hashtags, numbers, percentages, citations, person tags, phone numbers, times, and zip codes

#Create a sample data text file
#set seed to make the analysis reproducible
set.seed(52001)
small_Blogs <- englishBlogs [sample(1:length(englishBlogs), 1000)]
small_News <- englishNews[sample(1:length(englishNews), 1000)]
small_Twitter <- englishTwitter[sample(1:length(englishTwitter), 2000)]
small_Dataset <- c(small_Blogs, small_News, small_Twitter)

#Remove uneeded objects
rm(englishBlogs)
rm(englishNews)
rm(englishTwitter)
rm(small_Blogs)
rm(small_News)
rm(small_Twitter)

sampleTwitterDF_AFTER <- tolower(stripWhitespace(sampleTwitterDF))
small_Dataset_After <- tolower(stripWhitespace(small_Dataset))

sampleTwitterDF_AFTER <- rm_url(sampleTwitterDF_AFTER, clean = TRUE)
small_Dataset_After <- rm_url(small_Dataset_After, clean = TRUE)


small_Dataset_After <- rm_non_words(small_Dataset_After)

sampleTwitterDF_AFTER <- rm_non_ascii(sampleTwitterDF_AFTER)
small_Dataset_After <- rm_non_ascii(small_Dataset_After)#this takes a bit of time

sampleTwitterDF_AFTER[9]

sampleTwitterDF_AFTER <- replace_contraction(sampleTwitterDF_AFTER)
small_Dataset_After <- replace_contraction(small_Dataset_After)#this takes time

sampleTwitterDF_AFTER[1]

sampleTwitterDF_AFTER <- tolower(sampleTwitterDF_AFTER)
small_Dataset_After <- tolower(small_Dataset_After)
```

badWords <- readLines("~/Github/Coursera_DataScientist/Capstone/data/badWords.txt")

#To use the tm_map function, need to create a corpus
small_Dataset_After_Corpus <- VCorpus(VectorSource(small_Dataset_After), readerControl = list(reader=readPlain, language="english"))
small_Dataset_After_Corpus <- tm_map(small_Dataset_After_Corpus, removeWords, badWords)#takes time

sampleTwitterDF_AFTER_CORPUS <- VCorpus(VectorSource(sampleTwitterDF_AFTER), readerControl = list(reader=readPlain, language="english"))
sampleTwitterDF_AFTER_CORPUS <- tm_map(sampleTwitterDF_AFTER_CORPUS, removeWords, badWords)

sampleTwitterDF_AFTER <- as.data.frame(sampleTwitterDF_AFTER_CORPUS)
sampleTwitterDF_AFTER[9,2]

save(small_Dataset_After_Corpus, file="~/Github/Coursera_DataScientist/Capstone/data/small_Dataset_After_Corpus.txt")
save(sampleTwitterDF_AFTER_CORPUS, file="~/Github/Coursera_DataScientist/Capstone/data/sampleTwitterDF_AFTER_Corpus.txt")

rm(badWords)
rm(small_Dataset)
#rm(small_Dataset_After)
rm(sampleTwitterDF_AFTER)
rm(sampleTwitterDF_AFTER_CORPUS)

```
##Analyze the Text Data

textDoc <- DocumentTermMatrix(small_Dataset_After_Corpus)
transpose_textDoc <- TermDocumentMatrix(small_Dataset_After_Corpus)
word_frequency <- colSums(as.matrix(textDoc))   
#length(word_frequency)   
word_frequency_order <- order(word_frequency) 
textDoc_sparse <- removeSparseTerms(textDoc, 0.1)#Max 10% empty space
#inspect(textDoc_sparse)

#Most frequent terms are:
word_frequency[tail(word_frequency_order)]#most requent

#Some of the words that appear very infrequently:
word_frequency[head(word_frequency_order)]#least frequent

#Can create a data frame with this information - will use this for plotting
word_frequency_DF <- data.frame(word=names(word_frequency), freq=word_frequency)  

###Plot Word Frequency
library(ggplot2)   
p <- ggplot(subset(word_frequency_DF, freq>200), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

###Word Cloud
library(wordcloud)
library(RColorBrewer)
#display.brewer.all()
wordcloud(names(word_frequency), word_frequency, min.freq = 150, scale = c(6, 2),max.words=100, rot.per = 0.5, colors = brewer.pal(9, "Set1"))

##The N-Gram Tokenization

library(RWeka)
library(gridExtra)
ngram2 <- NGramTokenizer(small_Dataset_After, Weka_control(min=2, max=2, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram3 <- NGramTokenizer(small_Dataset_After, Weka_control(min=3, max=3, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram4 <- NGramTokenizer(small_Dataset_After, Weka_control(min=4, max=4, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram5 <- NGramTokenizer(small_Dataset_After, Weka_control(min=5, max=5, delimiters=" \\r\\n\\t.,;:\"()?!=$"))

ngram2_table <- data.frame(table(ngram2))
ngram2_table <- ngram2_table[order(ngram2_table$Freq,decreasing = TRUE),]
ngram2_table <- ngram2_table[1:10,]
colnames(ngram2_table) <- c("Word","Frequency")
ngramPlot2 <- ggplot(ngram2_table, aes(y=Frequency, x=Word, fill=Frequency) ) + 
geom_text(aes(label=Frequency), hjust=0) +
geom_bar(stat="Identity") +
coord_flip() +
labs(title="Bigram") +
theme(axis.text.x = element_text(vjust = 0)) +
scale_fill_distiller(palette="Spectral")

ngram3_table <- data.frame(table(ngram3))
ngram3_table <- ngram3_table[order(ngram3_table$Freq,decreasing = TRUE),]
ngram3_table <- ngram3_table[1:10,]
colnames(ngram3_table) <- c("Word","Frequency")
ngramPlot3 <- ggplot(ngram3_table, aes(y=Frequency, x=Word, fill=Frequency) ) + 
geom_text(aes(label=Frequency), hjust=0) +
geom_bar(stat="Identity") +
coord_flip() +
labs(title="Trigram") +
theme(axis.text.x = element_text(vjust = 0)) +
scale_fill_distiller(palette="Spectral")

ngram4_table <- data.frame(table(ngram4))
ngram4_table <- ngram4_table[order(ngram4_table$Freq,decreasing = TRUE),]
ngram4_table <- ngram4_table[1:10,]
colnames(ngram4_table) <- c("Word","Frequency")
ngramPlot4 <- ggplot(ngram4_table, aes(y=Frequency, x=Word, fill=Frequency) ) + 
geom_text(aes(label=Frequency), hjust=0) +
geom_bar(stat="Identity") +
coord_flip() +
labs(title="Quadgram") +
theme(axis.text.x = element_text(vjust = 0)) +
scale_fill_distiller(palette="Spectral")

ngram5_table <- data.frame(table(ngram5))
ngram5_table <- ngram5_table[order(ngram5_table$Freq,decreasing = TRUE),]
ngram5_table <- ngram5_table[1:10,]
colnames(ngram5_table) <- c("Word","Frequency")
ngramPlot5 <- ggplot(ngram5_table, aes(y=Frequency, x=Word, fill=Frequency) ) + 
geom_text(aes(label=Frequency), hjust=0) +
geom_bar(stat="Identity") +
coord_flip() +
labs(title="5-gram") +
theme(axis.text.x = element_text(vjust = 0)) +
scale_fill_distiller(palette="Spectral")

grid.arrange(ngramPlot2, ngramPlot3, ngramPlot3, ngramPlot5)

##References

# - [Helpful Guide using qdap](http://trinker.github.io/qdap/vignettes/qdap_vignette.html#clean0)
# - [Outstanding refernce doing basic text mining in R](https://rpubs.com/pjmurphy/31867)
# - [qdapRegex Guide](https://cran.r-project.org/web/packages/qdapRegex/qdapRegex.pdf)
# - [Outstanding documnet leading you rhrough text mining with R](http://onepager.togaware.com/TextMiningO.pdf)
# - [Another document on text mining in R - Helpful](http://www.unt.edu/rss/class/Jon/Benchmarks/TextMining_L_JDS_Jan2014.pdf)
# - Reliance on the guidance provided in the Coursera Course Discussion Forum

##Appendix - The R Code

########### OTHER cODE ####################
library(magrittr)
library(dplyr)
library(qdap)
library(qdapDictionaries)
library(ggplot2) 
library(scales) 
library(RColorBrewer)
library(Rgraphviz)#Correlation plots
library(qdapRegex)
library(RWeka)

library(tm)

getSources()
getReaders()

cname <- file.path(".", "Capstone/data/final/en_US", "./")
cname
dir(cname)

docs <- Corpus(DirSource(cname))
docs
class(docs)
summary(docs)
inspect(docs)

getTransformations()

################SEPERATE DOCS################
conBlogs <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.blogs.txt", "rb")#home
conNews <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.news.txt", "rb")
conTwitter <- file("C:/Users/czwea/Documents/GitHub/Capstone/data/rawData/final/en_US/en_US.twitter.txt", "rb")
#End of HOME cons


conBlogs <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.blogs.txt", "rb")
englishBlogs <- readLines(conBlogs)
close.connection(conBlogs)
rm(conBlogs)

conNews <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.news.txt", "rb")
englishNews <- readLines(conNews)
close.connection(conNews)
rm(conNews)

conTwitter <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.twitter.txt", "rb")
englishTwitter <- readLines(conTwitter, skipNul = TRUE)#Prevents warnings like In readLines(conTwitter):line 1759032 appears to contain an embedded nul
close.connection(conTwitter)
rm(conTwitter)

#Clean the data
blogs <- replace_contraction(englishBlogs)
news <- replace_contraction(englishNews)
twitter <- replace_contraction(englishTwitter)

rm(englishTwitter, englishNews, englishBlogs)

blogs <- rm_non_words(blogs)
news <- rm_non_words(news)
twitter<- rm_non_words(twitter)

blogs <- rm_non_ascii(blogs)
news <- rm_non_ascii(news)
twitter <- rm_non_ascii(twitter)

blogs <- rm_repeated_characters(blogs)
news <- rm_repeated_characters(news)
twitter <- rm_repeated_characters(twitter)

blogs <- rm_number(blogs)
news <- rm_number(news)
twitter <- rm_number(twitter)

blogs <- rm_white(blogs)
news <- rm_white(news)
twitter <- rm_white(twitter)

blogs <- L(blogs)
news <- L(news)
twitter <- L(twitter)

saveRDS(blogs, "blogs_cleaned.txt", compress = TRUE)
saveRDS(news, "news_cleaned.txt", compress = TRUE)
saveRDS(twitter, "twitter_cleaned.txt", compress = TRUE)# readRDS(file)

blogs <- readRDS("blogs_cleaned.txt")
blogs <- sample(blogs, 40000, replace=FALSE)

#Should I create a smaller sample file at this point?

blogs_corpus <- VCorpus(VectorSource(blogs), readerControl = list(reader=readPlain, language="english"))
inspect(blogs_corpus[3])
viewDocs(blogs_corpus, 3)

BigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

QuadgramTokenizer <-
     function(x)
          unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

blogs_tdm_2 <- TermDocumentMatrix(blogs_corpus, control = list(tokenize = BigramTokenizer))
blogs_tdm_3 <- TermDocumentMatrix(blogs_corpus, control = list(tokenize = TrigramTokenizer))
blogs_tdm_4 <- TermDocumentMatrix(blogs_corpus, control = list(tokenize = QuadgramTokenizer))

#inspect(removeSparseTerms(blogs_tdm_2[, 1:10], 0.9))
length(inspect(removeSparseTerms(blogs_tdm_4[,1:20], .1)))
inspect(blogs_tdm_4[,1:20])

blogs_dtm <- DocumentTermMatrix(blogs_corpus)




library(gridExtra)
ngram2 <- NGramTokenizer(blogs_corpus, Weka_control(min=2, max=2, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram3 <- NGramTokenizer(small_Dataset_After, Weka_control(min=3, max=3, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram4 <- NGramTokenizer(small_Dataset_After, Weka_control(min=4, max=4, delimiters=" \\r\\n\\t.,;:\"()?!=$"))
ngram5 <- NGramTokenizer(small_Dataset_After, Weka_control(min=5, max=5, delimiters=" \\r\\n\\t.,;:\"()?!=$"))

# blogs <- readRDS("blogs_contracted.txt")
# news <- readRDS("news_contracted.txt")
# twitter <- readRDS("twitter_contracted.txt")

# blogs <- tm_map(as.Corpus(blogs), toSpace, "/|@|\\|(/)")#Taken care of with code above (no words and ascii)
# news <- tm_map(as.Corpus(news), toSpace, "/|@|\\|(/)")
# twitter <- tm_map(as.Corpus(twitter), toSpace, "/|@|\\|(/)")

# blogs <- tm_map(as.Corpus(blogs), content_transformer(tolower))
# news <- tm_map(as.Corpus(news), content_transformer(tolower))
# twitter <- tm_map(as.Corpus(twitter), content_transformer(tolower))
# 
# blogs <- tm_map(blogs, removeNumbers)
# news <- tm_map(news, removeNumbers)
# twitter <- tm_map(twitter, removeNumbers)

# blogs <- tm_map(blogs, removePunctuation)
# news <- tm_map(news, removePunctuation)
# twitter <- tm_map(twitter, removePunctuation)

# blogs <- tm_map(blogs, stripWhitespace)
# news <- tm_map(news, stripWhitespace)
# twitter <- tm_map(twitter, stripWhitespace)

# saveRDS(twitter, "twitter_transformed.txt", compress = TRUE)
# saveRDS(news, "news_transformed.txt", compress = TRUE)
# saveRDS(blogs, "blogs_transformed.txt", compress = TRUE)

# blogs <- readRDS("blogs_transformed.txt")
# news <- readRDS("news_transformed.txt")
# twitter <- readRDS("twitter_transformed.txt")

####Create DTM#############
# dtm_blogs <- DocumentTermMatrix(blogs)
# dtm_news <- DocumentTermMatrix(news)
# dtm_twitter <- DocumentTermMatrix(twitter)
# 
# write.csv(as.matrix(dtm_blogs), file="dtm_blogs.csv")
# write.csv(as.matrix(dtm_news), file="dtm_news.csv")
# write.csv(as.matrix(dtm_twitter), file="dtm_twitter.csv")
# 
# dim(dtm_blogs)
# inspect(dtm_blogs[1:1, 250000:259000])#Not SparseM
# 
# dim(dtm_news)
# inspect(dtm_news[1, 220000:227200])#Not SparseM
# 
# dim(dtm_twitter)
# inspect(dtm_blogs[1, 310000:310700])#Not Working
# 
# freq_blogs <- colSums(as.matrix(dtm_blogs))
# length(freq_blogs)
# order_blogs <- order(freq_blogs)
# freq_blogs[head(order_blogs)]#least freqquent
# freq_blogs[tail(order_blogs)]#most frequent
# head(table(freq_blogs), 15)#frequency of frequencies - 276744 terms occur only once
# 
# #remove terms not used frequently
# #dtm_blogs_sparse1 <- removeSparseTerms(dtm_blogs, 0.1)#see what the size diff is
# dtm_blogs_sparse2 <- removeSparseTerms(dtm_blogs, 0.2)
# #dtm_blogs_sparse5 <- removeSparseTerms(dtm_blogs, 0.5)
# 
# 
# freq_blogs_sparse2 <- colSums(as.matrix(dtm_blogs_sparse2))
# freq_blogs_sparse2
# ## data graham inform time use william ## 3101 108 467 483 1366 236
# table(freq_blogs_sparse2)
# 
# findFreqTerms(dtm_blogs, lowfreq=20000)
# 
# #I did not get any returns with blogs
# findAssocs(dtm_blogs, "about", corlimit=0.2)#If two words always appear together then the correlation would be 1.0 
#and if they never appear together the correlation would be 0.0. Thus the correlation is a measure of how closely 
#associated the words are in the corpus.


#Spelling
# library(hunspell)#https://www.opencpu.org/posts/hunspell-release/
# blogs <- as.wfm(dtm_blogs)
# blogs <- as.data.frame(blogs)
# head(blogs)
# blogs.words <- row.names(blogs)
# blogs$word <- blogs.words


################

viewDocs <- function(d, n) {d %>% extract2(n) %>% as.character() %>% writeLines()} #extract2 part of magrittr
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

#####################

stopwords("english")

