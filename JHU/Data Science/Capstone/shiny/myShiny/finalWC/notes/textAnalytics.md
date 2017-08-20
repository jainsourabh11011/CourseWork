# Text Analytics

##What is text analytics (or text mining)?

Text mining is the analysis of data contained in natural language text. The application of text mining techniques to solve business problems is called text analytics.

##Text Analytics - Who cares?

Text analytics can help companies obtain valuable business insights from word documents, email and social media streams like Facebook, Twitter and LinkedIn. Software can help by transposing words and phrases in unstructured data into numerical values which can then be linked with structured data and analyzed with a variety of data, natural language process and machine learning tools. 

> “There is going to be a boom for design companies, because there’s going to be so much information people have to work through quickly,” said Diane B. Greene, the head of Google Compute Engine, one of the companies hoping to steer an A.I. boom. “Just teaching companies how to use A.I. will be a big business.”

##How Do You Do This?

The development of a text analysis solution starts with the concept of *n*-grams. An *n*-gram is a contiguous sequence of n items from a given sequence of text or speech.  The analysis of *n*-grams gives information which words are more likely to be found combined in a sentence. *n*-grams are typically called unigrams, bigrams, trigrams, etc. depending upon the number of words selected. A collection of *n*-grams accompanied by the frequency in which these pairs of words appear provide the fundamental building blocks of my application. The *n*-gram model is one of the most important tools in speech and language processing. 

###It's All About Probabilities

Let's see how to use *n*-gram models to estimate the probability of the last word of an *n*-gram given the previous words and assign probabilities to the word sequence.  

We explore this concept with an example borrowed from [Daniel Jurafsky and James Martin](https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf).  Let’s compute the probability of a word w given some history h - P(w|h). Suppose the history h is “its water is so transparent that” and we want to know the probability that the next word is *the*.  This can be expressed::

<center>P(the|its water is so transparent that)</center>
<br>
One way is to estimate this probability is from relative frequency counts: take a very large corpus, count the number of times we see its water is so transparent that and count the number of times this is followed by the. This would be answering the question “Out of the times we saw the history h,how many times was it followed by the word w”, as follows: 

<center>P(the|its water is so transparent that) = C(its water is so transparent that the) / C(its water is so transparent that)</center>
<br>
With a large enough corpus we can compute these counts and estimate the probability. Once we know to probabilities of the *n*-grams, it becomes a pretty simple exercise to compare the entered words to find the next word with the greatest probability according the to *n*-grams.

##Here's of list of helpful resources

Want to learn more?  Here are some helpful resources.

- [Speech and Language Processing: An introduction to natural language processing (Jurafsky & Martin, 2007)](http://stp.lingfil.uu.se/~santinim/ml/2014/JurafskyMartinSpeechAndLanguageProcessing2ed_draft%202007.pdf) 
- [n-gram paper](https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf)
- [n-gram models](http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf)
- [Introduction to generative models of language](http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/n-gram-models-2.pdf)
- [Markov Chains Visual explanation](http://setosa.io/ev/markov-chains/)
- [Markov Chain Vignette](https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf)
- [Profanity/Bad Words](http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/)
- [Bad Words from Google](https://code.google.com/archive/p/badwordslist/downloads)
- [Smoothing Tutorial](http://nlp.stanford.edu/%7Ewcmac/papers/20050421-smoothing-tutorial.pdf)
- [Outstanding refernce doing basic text mining in R](https://rpubs.com/pjmurphy/31867)
- [Outstanding documnet leading you through text mining with R](http://onepager.togaware.com/TextMiningO.pdf)
- [Another document on text mining in R - Helpful](http://www.unt.edu/rss/class/Jon/Benchmarks/TextMining_L_JDS_Jan2014.pdf)

###Relevant R Packages
- [Good quanteda tutorial/exercise](http://www.kenbenoit.net/courses/nyu2014qta/exercise1.html)
- [Quanteda Vignette/Quick Start](https://cran.rstudio.com/web/packages/quanteda/vignettes/quickstart.html)
- [Qdap](https://cran.r-project.org/web/packages/qdap/qdap.pdf)
- [Helpful Guide using qdap](http://trinker.github.io/qdap/vignettes/qdap_vignette.html#clean0)
- [qdapRegex Guide](https://cran.r-project.org/web/packages/qdapRegex/qdapRegex.pdf)
- [Introduction to tm](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf)

