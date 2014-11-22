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
# textsS <- texts[sampleIndex[[1]]]
textsS <- llply(sampleIndex, function(index) texts[index])
rm(sampleIndex)

###
# ngram functions
###

getNgrams <- function(texts_list, n) {
    NgramTokenizerFUN <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    ngrams <- llply(texts_list, NgramTokenizerFUN)
    ngrams <- do.call(c, ngrams)
#     ngrams = NULL
#     for (i in seq_along(texts_list)) {
#         ngrami <- NgramTokenizerFUN(texts_list[[i]])
#         ngrams <- c(ngrams, ngrami)
#     }
    return(ngrams)
}

getNgramCount <- function(ngrams) {
    # get ngram counts in decreasing order
    ngramCount <- sort(table(ngrams), decreasing = T)
    # remove large number of ngrams with counts of 1
    ngramCount <- ngramCount[ngramCount > 1]
    # remove ngrams with digits
    ngramCount <- ngramCount[!grepl("[[:digit:]]", names(ngramCount))]
    return(ngramCount)
}

getNgramProp <- function(ngramCount) {
    ngramCountNum <- rev(table(ngramCount))
    ngramProp <- data.frame(threshold = as.numeric(names(ngramCountNum)),
                            count = ngramCountNum,
                            uniqueProp = cumsum(ngramCountNum) / sum(ngramCountNum))
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
                           n = n)
    rownames(ngram.df) <- NULL
    ngram.df <- mutate(ngram.df,
                       ngram = paste(words, suggestion))
    return(ngram.df)
}

reduceNgramDF <- function(ngramdf) {
    ngramCount <- table(ngramdf$words)
    ngramRemove <- names(ngramCount)[ngramCount == 1]
    subset(ngramdf, !is.element(words, ngramRemove))
}

getWordsProp <- function(df) {
    ddply(df, .(words), 
          function(sdf) {sdf <- mutate(sdf, prop = count / sum(count))
                         sdf <- arrange(sdf, desc(prop))
                         head(sdf, 3)}
    )
}

###
# bigram
###

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
system.time(bigrams <- getNgrams(textsS, 2))
system.time(bigramCount <- getNgramCount(bigrams))
rm(bigrams)
system.time(bigramProp <- getNgramProp(bigramCount))
system.time(bigram.df <- getNgramDF(bigramCount))
system.time(bigram.df <- reduceNgramDF(bigram.df))
# save(bigram.df, file = "RData/bigramdf.RData")
# write.table(bigram.df, file = "RData/bigramdf.txt", row.names = F)

###
# unigram
###

unigramCount <- sort(table(bigram.df$suggestion), decreasing = T)
unigram.df <- data.frame(suggestion = names(unigramCount), count = unigramCount)
# save(unigram.df, file = "RData/unigramdf.RData")

###
# trigram
###

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
system.time(trigrams <- getNgrams(textsS, 3))
system.time(trigramCount <- getNgramCount(trigrams))
rm(trigrams)
system.time(trigramProp <- getNgramProp(trigramCount))
system.time(trigram.df <- getNgramDF(trigramCount))
system.time(trigram.df <- reduceNgramDF(trigram.df))
# save(trigram.df, file = "RData/trigramdf.RData")
# write.table(trigram.df, file = "RData/trigramdf.txt", row.names = F)

###
# quadgram
###

# QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
# system.time(quadgrams <- getNgrams(textsS, 4))
# system.time(quadgramCount <- getNgramCount(quadgrams))
# rm(quadgrams)
# system.time(quadgramProp <- getNgramProp(quadgramCount))
# system.time(quadgram.df <- getNgramDF(quadgramCount))
# save(quadgram.df, file = "RData/quadgramdf.RData")
# write.table(quadgram.df, file = "RData/quadgramdf.txt", row.names = F)

###
# load ngram dfs
###

if(!is.element("unigram.df", ls())) load("RData/unigramdf.RData")
if(!is.element("bigram.df", ls())) load("RData/bigramdf.RData")
if(!is.element("trigram.df", ls())) load("RData/trigramdf.RData")

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

###
# ngram subset
###

bigram.df2 <- getWordsProp(bigram.df)
save(bigram.df2, file = "RData/bigramdf2.RData")

trigram.df2 <- getWordsProp(trigram.df)
save(trigram.df2, file = "RData/trigramdf2.RData")
