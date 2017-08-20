paste("hello", "world")
paste("Today is", date())

paste(c("a","b"), c(1,2)) 

paste("A", 1:5)

# Create a list of six unigrams
uni = c("a", "screaming", "comes", "across", "the", "sky")
# Remove the first element and add a period at the end
uni2 <- c(uni[-1], ".")

uni2 


# Find all bigrams using paste
paste(uni, uni2) # Notice there are six 

uni2

uni3 <- c(uni2[-1], ".")

uni3

# Find all trigrams using paste
paste(uni, uni2, uni3)

# Read in the text
bible <- scan(what="c", sep="\n", file="./Capstone/data/KJV12.TXT") 
# Convert to lower case
bible <- tolower(bible)

# Delete chapter:verse numbers like 1:1 when tokenizing
tokens <- unlist(strsplit(bible, "[^a-z]+"))

# Remove empty tokens
tokens <- tokens[tokens != ""]

# Compute N, the number of tokens
length(tokens)

# Remove the first element and add a period at the end
tokens2 <- c(tokens[-1], ".")

# Note the length stays the same
length(tokens2) 

# Create a sorted table of bigram type frequencies
freq <- sort(table(paste(tokens, tokens2)), decreasing=T)
head(freq)

for (b in names(freq)[1:15]) { cat(b, freq[b], freq[b]/791842, "\n", sep="\t") };

# Again, remove the first element and add a period at the end
tokens3 <- c(tokens2[-1], ".")

# Create a vector of trigrams using paste
trigrams <- paste(tokens, tokens2, tokens3)

# Create a sorted table of trigram type frequencies
freq <- sort(table(trigrams), decreasing=T)
head(freq)

for (t in names(freq)[1:15]) { cat(t, freq[t], freq[t]/791842, "\n", sep="\t") }






