# Load trigrams for accuracy extimation
#
# Author: filqua74

library(dplyr)
library(data.table)

trigrams_test <- as.data.table(read.csv("corpora/corpora_ascii.test_3grams.txt", header = F, stringsAsFactors = F, colClasses = c(rep("character",3))))

numSample <- 300

res <- apply(trigrams_test[sample(nrow(trigrams_test),numSample)], 1, function(x) x[3] %in% wordPredict(paste(x[1],x[2],sep=" "),ncand=10)$word)

acc <- sum(res)/numSample

print(paste("Accuracy=", acc))