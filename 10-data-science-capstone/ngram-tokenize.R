setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

library(plyr)
library(RWeka)
library(wordcloud); library(RColorBrewer)

###
# sampling
###

if(!is.element("texts", ls())) load("RData/texts.RData")
set.seed(0)
sampleIndex <- split(sample(seq_along(texts)), rep_len(1:100, length(texts)))
textsS <- llply(sampleIndex, function(index) texts[index])
rm(texts, sampleIndex)

###
# ngram functions
###

getNgrams <- function(texts_list, n) {
    NgramTokenizerFUN <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    # use of loops in tokenization to reduce memory requirements
    ngrams = NULL
    for (i in seq_along(texts_list)) {
        ngrami <- NgramTokenizerFUN(texts_list[[i]])
        ngrams <- c(ngrams, ngrami)
    }
    return(ngrams)
}

getNgramCount <- function(ngrams) {
    # get ngram counts in decreasing order
    ngramCount <- sort(table(ngrams), decreasing = T)
    # remove large number of ngrams with counts of 1
    ngramCount <- ngramCount[ngramCount > 1]
    # remove ngrams ending with numbers
    ngramCount <- ngramCount[!grepl("<NUM>$", names(ngramCount))]
    return(ngramCount)
}

getNgramProp <- function(ngramCount) {
    ngramCountNum <- rev(table(ngramCount))
    ngramProp <- data.frame(threshold = as.numeric(names(ngramCountNum)),
                            count = ngramCountNum,
                            uniqueProp = cumsum(ngramCountNum) / sum(ngramCountNum),
                            stringsAsFactors = F)
    ngramProp <- mutate(ngramProp, 
                        totalCount = threshold * count,
                        totalProp = cumsum(totalCount) / sum(totalCount))
    return(ngramProp)
}

getNgramDF <- function(ngramCount) {
    ngrams <- do.call(rbind, strsplit(names(ngramCount), " "))
    n <- ncol(ngrams)
    ngram.df <- data.frame(words = do.call(paste, as.data.frame(ngrams[,-n])),
                           suggestion = ngrams[,n],
                           count = ngramCount,
                           n = n,
                           stringsAsFactors = F)
    rownames(ngram.df) <- NULL
    ngram.df <- mutate(ngram.df,
                       ngram = paste(words, suggestion))
    return(ngram.df)
}

reduceNgramDF <- function(ngramdf) {
    ngramCount <- table(ngramdf$words)
    ngramRemove <- names(ngramCount)[ngramCount == 1]
    ngramdf <- subset(ngramdf, !is.element(words, ngramRemove))
    return(ngramdf)
}

getWordsProp <- function(df) {
    df <- ddply(df, .(words), 
                function(sdf) {sdf <- mutate(sdf, prop = count / sum(count))
                               sdf <- arrange(sdf, desc(prop))
                               head(sdf, 3)}
                )
    df <- arrange(df, desc(count))
    return(df)
}

###
# bigram
###

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
system.time(bigrams <- getNgrams(textsS, 2))
rm(textsS)
system.time(bigramCount <- getNgramCount(bigrams))
rm(bigrams)
system.time(bigramProp <- getNgramProp(bigramCount))
system.time(bigram.df <- getNgramDF(bigramCount))
system.time(bigram.df <- reduceNgramDF(bigram.df))
# save(bigram.df, file = "RData/bigramdf.RData")

system.time(bigram.df2 <- getWordsProp(bigram.df))
# save(bigram.df2, file = "RData/bigramdf2.RData")

###
# unigram
###

unigramCount <- sort(table(bigram.df$suggestion), decreasing = T)
unigram.df <- data.frame(suggestion = names(unigramCount), count = unigramCount,
                         prop = unigramCount / sum(unigramCount), n = 1,
                         stringsAsFactors = F)
# save(unigram.df, file = "RData/unigramdf.RData")

###
# trigram
###

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
system.time(trigrams <- getNgrams(textsS, 3))
rm(textsS)
system.time(trigramCount <- getNgramCount(trigrams))
rm(trigrams)
system.time(trigramProp <- getNgramProp(trigramCount))
system.time(trigram.df <- getNgramDF(trigramCount))
system.time(trigram.df <- reduceNgramDF(trigram.df))
# save(trigram.df, file = "RData/trigramdf.RData")

trigram.df2 <- getWordsProp(trigram.df)
# save(trigram.df2, file = "RData/trigramdf2.RData")

###
# quadgram
###

# QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
# system.time(quadgrams <- getNgrams(textsS, 4))
# rm(textsS)
# system.time(quadgramCount <- getNgramCount(quadgrams))
# rm(quadgrams)
# system.time(quadgramProp <- getNgramProp(quadgramCount))
# system.time(quadgram.df <- getNgramDF(quadgramCount))
# save(quadgram.df, file = "RData/quadgramdf.RData")

quadgram.df2 <- getWordsProp(quadgram.df)
# save(quadgram.df2, file = "RData/quadgramdf2.RData")

###
# load ngram dfs
###

if(!is.element("unigram.df", ls())) load("RData/unigramdf.RData")
if(!is.element("bigram.df", ls())) load("RData/bigramdf.RData")
if(!is.element("trigram.df", ls())) load("RData/trigramdf.RData")
# if(!is.element("quadgram.df", ls())) load("RData/quadgramdf.RData")

if(!is.element("bigram.df2", ls())) load("RData/bigramdf2.RData")
if(!is.element("trigram.df2", ls())) load("RData/trigramdf2.RData")

###
# word cloud
###

with(bigram.df, 
     wordcloud(ngram, count,
               max.words = 200,
               colors = brewer.pal(9, "Set1"))
)

with(trigram.df, 
     wordcloud(ngram, count,
               scale = c(3,.5), max.words = 100,
               colors = brewer.pal(9, "Set1"))
)

# with(quadgram.df, 
#      wordcloud(ngram, count,
#                scale = c(3,.5), max.words = 50,
#                colors = brewer.pal(9, "Set1"))
# )
