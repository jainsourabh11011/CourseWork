
library(wordcloud)
wordClound <- wordcloud(unique(Data1Grams$unigram), Data1Grams$frequency, min.freq = 250, 
                        scale = c(4, 0.5),max.words=100, rot.per = 0.5, colors = brewer.pal(9, "Set1"))

noStopWords <- readRDS("./data/UnigramNoStopWordsDF.RDS")

wordCloud2 <- wordcloud(noStopWords$String, Data1Grams$frequency, min.freq = 250, 
                        scale = c(4, 0.5),max.words=100, rot.per = 0.5, colors = brewer.pal(9, "Set1"))

