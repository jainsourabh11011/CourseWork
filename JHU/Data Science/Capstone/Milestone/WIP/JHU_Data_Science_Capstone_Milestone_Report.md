# JHU Data Science Capstone Milestone Report
Cliff Weaver  
###Introduction

This document satisfies one of the course requirements for the [Johns Hopkins Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) hosted by Coursera.  The specialization consists of 9 courses finishing with a 10th Capstone Project.  This document is a requirement for the Capstone Project.  

The Capstone Project [^1] analyzes a corpus of text documents to identify the structure in the data and how words are put together. It includes cleaning and analyzing text data, sampling and building a predictive text model. Lastly, a predictive data product in Shiny will be developed.  This exercise  demonstrates many of the skills developed during the Data Science Specialization.  These skills include:

- **Understanding the problem**
- **Data acquisition and cleaning**
- **Exploratory analysis**
- Statistical modeling
- Predictive modeling
- Creative exploration
- Creating a data product
- Creating a short slide deck pitching your product
The **bolded tasks** are satisfied by this report.

The specific requirements for this report are:

1. Demonstrate that you've downloaded the data and have successfully loaded it in.
2. Create a basic report of summary statistics about the data sets.
3. Report any interesting findings that you amassed so far.
4. Get feedback on your plans for creating a prediction algorithm and Shiny app. 

##Download the Data
Let's explore the data for the Capstone Project.  Johns Hopkins has partnered with [Swiftkey](https://swiftkey.com/en) to provide textual data sources found [here](http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The data includes blogs, news and twitter files in English, German, Finish, and Russian. This data will be used to build an English Natural Language Processing predictive to predict the next word based on previous words.

Let's start by downloading the data and naming the text files.

```r
library(downloader)
library(qdap)
if(!file.exists("../Capstone/data/rawData.zip")){
     fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
     download(fileURL, dest = "../Capstone/data/rawData.zip")
     unzip("../Capstone/data/rawData.zip", exdir="../Capstone/data")#folders:  data/final/en_US
}
```
Here are the English files we will be evaluating:

```r
fileNames <- list.files("../Capstone/data/final/en_US/")
```

```r
#Would never had known how to do this without this entry in the course discussions
##https://www.coursera.org/learn/data-science project/discussions/WB6O7eb9EeWhmg4JHc-OZw?sort=createdAtDesc&page=1
##Unable to use relative path with file
conBlogs <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.blogs.txt", "rb")
conNews <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.news.txt", "rb")
conTwitter <- file("C:/Users/cweaver/Documents/GitHub/Coursera_DataScientist/Capstone/data/final/en_US/en_US.twitter.txt", "rb")

englishBlogs <- readLines(conBlogs)
englishNews <- readLines(conNews)
englishTwitter <- readLines(conTwitter)
```

```
## Warning in readLines(conTwitter): line 167155 appears to contain an
## embedded nul
```

```
## Warning in readLines(conTwitter): line 268547 appears to contain an
## embedded nul
```

```
## Warning in readLines(conTwitter): line 1274086 appears to contain an
## embedded nul
```

```
## Warning in readLines(conTwitter): line 1759032 appears to contain an
## embedded nul
```

```r
close(conBlogs)
close(conNews)
close(conTwitter)
```

I have named these files  *englishBlogs*, *englishNews* and *englishTwitter*.  Now that we have capure the raw data, it is time to explore and clean the data using text analytics - somestimes called text mining - methodologies.

Let's see a few random lines from each file:

```r
#Blog File
head(sample(englishBlogs, 3))
```

```
## [1] "The funny thing is, she went half-way across the world to study the issues that beset her birth country. See a description of her research on her blog Moving Malaysians."                                                   
## [2] "Aretha Franklin â Save Me"                                                                                                                                                                                                 
## [3] "It's bound to come, the show where they find someone buried beneath 500 1.75 L bottles of Old Grand-Dad from the National Distillers distillery in Frankfort, circa 1985. (That's the sort of thing whiskey hoarders hoard.)"
```

```r
#News File
head(sample(englishNews, 3))
```

```
## [1] "12 ounces flank steak, cut diagonally across grain into thin slices"                                                                            
## [2] "* San Francisco will take pity on kids and let them get a toy with their Happy Meals."                                                          
## [3] "Raynaud was 3 for 4 with a double and scored three runs while knocking in another to lead the Wildcats to an 8-0 win over Downey last Saturday."
```

```r
#Twitter File
head(sample(englishTwitter, 3))
```

```
## [1] "Don't know what I'm signing up for, but sure! :)"          
## [2] "me too!!!!!!! So soothing"                                 
## [3] "Oh dear, Glasgow managers dont dominate the EPL after all."
```

Here is some file summary information:

```r
#library(DT)#http://rstudio.github.io/DT/
library(stringi)#https://cran.r-project.org/web/packages/stringi/stringi.pdf
#library(R.utils)
#library(qdap)

# txtColnames <-c("File Name", "File Size", "Rows", "No Words")
# txtRows <- c("englishTwitter", "englishNews", "englishBlogs")
# 
fileSizeBlogs <- object.size(englishBlogs)/1024^2
fileSizeNews <- object.size(englishNews)/1024^2
fileSizeTwitter <- object.size(englishTwitter[3])/1024^2

numOflinesBlogs <- length(englishBlogs)
numOflinesNews <- length(englishNews)
numOflinesTwitter <- length(englishTwitter)
# 
# #numOflineTEST <- unlist(stri_extract_all_words(TESTfile, locale = "en"))
# 
numOfwordsBlogs <- length(unlist(stri_extract_all_words(englishBlogs, locale = "en")))
numOfwordsNews <- length(unlist(stri_extract_all_words(englishNews, locale = "en")))
numOfwordsTwitter <- length(unlist(stri_extract_all_words(englishTwitter, locale = "en")))
```
File Name | File Size | Lines Count | No of Words
--------- | --------- | ----------- | -----------
English Blogs | 248.4934998 | 899288 | 38154241
English News | 249.6328812 | 1010242 | 35010782
English Twitter | 1.2969971\times 10^{-4} | 2360148 | 30218125

Text Analytics applies analytic tools to learn from collections of text data, like social media, books, newspapers, and emails. By using automated algorithms massive amounts of text can be analyzed or learned. The material could consist of millions of newspaper articles to perhaps summarise the main themes and to identify those that are of most interest.  By leveraging data science, much greater amounts of data can be consumed compared to individual human potential.

The primary package for text mining, tm [^2] , provides a framework within which we perform our text mining. In addition to tm, other packages will be used to augment tm.  These additional packages will be noted as they are used.

In order to enable faster data processing, a data sample from all three sources was generated.







# Summary Statistics









The following table provides an overview of the imported data. In addition to the size of each data set, the number of lines and words are displayed. 





A word cloud usually provides a first overview of the word frequencies. The word cloud displays the data of the aggregated sample file.


```r
# trigramTDM <- TermDocumentMatrix(finalCorpus)
# wcloud <- as.matrix(trigramTDM)
# v <- sort(rowSums(wcloud),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# wordcloud(d$word,d$freq,
#           c(5,.3),50,
#           random.order=FALSE,
#           colors=brewer.pal(8, "Dark2"))
```


# Building A Clean Text Corpus

By using the [tm package](http://tm.r-forge.r-project.org/index.html) the sample data gets *cleaned*. With cleaning it is meant that the text data is converted into lower case, further punctuation, numbers and URLs are getting removed. Next to that stop and profanity words are erased from the text sample. At the end we are getting a clean text corpus which enables an easy subsequent processing.

The used profanity words can be inspected [in this Github Repository](https://github.com/mhnierhoff/CapstoneCoursera/blob/master/MilestoneReport/profanityfilter.txt).


```r
## Make it work with the new tm package
# cleanSample <- tm_map(cleanSample, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")), 
#                       mc.cores=2)
# cleanSample <- tm_map(cleanSample, content_transformer(tolower), lazy = TRUE)
# cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
# cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
# removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
# cleanSample <- tm_map(cleanSample, content_transformer(removeURL))
# cleanSample <- tm_map(cleanSample, stripWhitespace)
# cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
# cleanSample <- tm_map(cleanSample, removeWords, profanityWords)
# cleanSample <- tm_map(cleanSample, stemDocument)
# cleanSample <- tm_map(cleanSample, stripWhitespace)
```




## The N-Gram Tokenization

In Natural Language Processing (NLP) an *n*-gram is a contiguous sequence of n items from a given sequence of text or speech.

The following function is used to extract 1-grams, 2-grams and 2-grams from the cleaned text corpus.


```r
# ngramTokenizer <- function(theCorpus, ngramCount) {
#         ngramFunction <- NGramTokenizer(theCorpus, 
#                                 Weka_control(min = ngramCount, max = ngramCount, 
#                                 delimiters = " \\r\\n\\t.,;:\"()?!"))
#         ngramFunction <- data.frame(table(ngramFunction))
#         ngramFunction <- ngramFunction[order(ngramFunction$Freq, 
#                                              decreasing = TRUE),][1:10,]
#         colnames(ngramFunction) <- c("String","Count")
#         ngramFunction
# }
```

By the usage of the tokenizer function for the *n*-grams a distribution of the following top 10 words and word combinations can be inspected. Unigrams are single words, while bigrams are two word combinations and trigrams are three word combinations.

### Top Unigrams

```r
# unigram <- readRDS("./unigram.RDS")
# unigramPlot <- gvisColumnChart(unigram, "String", "Count",                  
#                             options=list(legend="none"))
# 
# print(unigramPlot, "chart")
```

### Top Bigrams

```r
# bigram <- readRDS("./bigram.RDS")
# bigramPlot <- gvisColumnChart(bigram, "String", "Count",                  
#                             options=list(legend="none"))
# 
# print(bigramPlot, "chart")
```

### Top Trigrams

```r
# trigram <- readRDS("./trigram.RDS")
# trigramPlot <- gvisColumnChart(trigram, "String", "Count",                  
#                             options=list(legend="none"))
# 
# print(trigramPlot, "chart")
```


# Interesting Findings

+ Loading the dataset costs a lot of time. The processing is time consuming because of the huge file size of the dataset. By avoiding endless run times of the code, it was indispensable to create a data sample for text mining and tokenization. Needless to say, this workaround decreases the accuracy for the subsequent predictions.

+ Removing all stop-words from the corpus is recommended, but, of course, stop-words are a fundamental part of languages. Therefore, consideration should be given to include these stop words in the prediction application again.

+ The text mining algorithm needs to be adjusted, so to speak a kind of fine-tuning. As seen in the chart of the top trigrams some words severely curtailed. For example, the second most common trigram is *presid barack obama* instead of *president barack obama*.

# Next Steps For The Prediction Application

As already noted, the next step of the capstone project will be to create a prediction application. 
To create a smooth and fast application it is absolutely necessary to build a fast prediction algorithm. This is also means, I need to find ways for a faster processing of larger datasets. Next to that,  increasing the value of n for n-gram tokenization will improve the prediction accuracy. All in all a shiny application will be created which will be able to predict the next word a user wants to write.

# All Used Code Scripts

All used code snippets to generate this report can be viewed in this [repository](https://github.com/mhnierhoff/CapstoneCoursera/tree/master/MilestoneReport).

# Session Informations

```r
#sessionInfo()
```

[^1]: Testing Footnotes
[^2]: Feinerer and Hornik, 2015

If you're wondering why you should keep only short subchains instead of full chains, then look into the theory of [Markov windows](https://en.wikipedia.org/wiki/Markov_property). If your model were to remember all the chains of words that it has seen in its input, then it would badly overfit its training data and only reproduce its input at prediction time. How badly depends on the training set (more data is better), but for k>4 you'd really need smoothing in your model.
