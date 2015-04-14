library(plyr)
library(RWeka)

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