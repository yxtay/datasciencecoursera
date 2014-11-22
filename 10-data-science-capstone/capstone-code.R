setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

###
# load packages
###

library(plyr); library(reshape2)
library(ggplot2); library(gridExtra)
library(stringr); library(stringi); library(RWeka)
library(tm); library(SnowballC); library(qdap)
library(wordcloud); library(RColorBrewer)
library(knitr); library(beepr)
library(doParallel); library(foreach)
# beep(2)

###
# parallel computing
###

cl <- makeCluster(detectCores())
registerDoParallel(cl)

###
# read text
###

name <- c("blogs", "news", "twitter")
files <- paste0("./final/en_US/en_US.", name, ".txt")
system.time(texts <- llply(files, readLines, encoding = "UTF-8", skipNul = T))
names(texts) <- name
texts <- llply(texts, iconv, from = "UTF-8", to = "UTF-8", sub = "")
rm(name, files)

###
# explore
###

docSize <- sapply(texts, object.size)
lineCount <- sapply(texts, length)
charCount <- llply(texts, nchar)
meanChar <- sapply(charCount, mean)
df.exp <- data.frame(lineCount, docSize, meanChar)

system.time(lineCount <- llply(texts, length))
qplot(L1, value, data = melt(lineCount), 
      geom = "bar", stat = "identity")

system.time(words <- llply(texts, strsplit, split = " "))
system.time(wordCount <- llply(words, laply, length))
df.wc <- melt(wordCount)
names(df.wc) <- c("count", "source")

p1 <- qplot(count, data = df.wc, geom = "histogram", fill = source,
            xlim = c(0, 150),
            xlab = "Word Count", ylab = "Frequency")

p2 <- qplot(count, data = subset(df.wc, source == "blogs"), 
            xlim = c(0, 210),
            main = "Blogs",
            xlab = "Word Count", ylab = "Frequency")

p3 <- qplot(count, data = subset(df.wc, source == "news"), 
            xlim = c(0, 120),
            main = "News",
            xlab = "Word Count", ylab = "Frequency")

p4 <- qplot(count, data = subset(df.wc, source == "twitter"),
            xlim = c(0, 40), binwidth = 2,
            main = "Twitter",
            xlab = "Word Count", ylab = "Frequency")

grid.arrange(p1, p2, p3, p4, ncol = 2)

###
# Quiz 1
###

charCount <- llply(texts, nchar)
llply(charCount, max)
length(grep("love", texts[[3]]))/length(grep("hate", texts[[3]]))
grep("biostats", texts[[3]], value = T)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", texts[[3]]))

###
# samepling
###

set.seed(0)
lineCount <- llply(texts, length)
sampleIndex <- llply(llply(lineCount, seq_len), sample)
sampleClass <- llply(lineCount, rep_len, x = 1:10)
textsS2 <- mapply(function(text, index, class) {
    text[index[class == 1]]
}, texts, sampleIndex, sampleClass, SIMPLIFY = F)
textsS <- llply(seq_along(texts), 
                function(i) sample(texts[[i]], ceiling(0.1 * lineCount[[i]])))
system.time(textsS <- do.call(c, textsS))

###
# preprocessing
###
x <- gsub("[[:print:]]+", "", textsS)
x <- unlist(strsplit(x, ""))

# replace certain non-printable characters with printable counterparts
textsS <- gsub("[–—―‒\u0096]", "-", textsS)
textsS <- gsub("[“”″\u0093\u0094]", '"', textsS)
textsS <- gsub("[…\u0085]", "...", textsS)
textsS <- gsub("[‘’′'′\u0092]", "'", textsS)
textsS <- gsub("[：]", ":", textsS)
textsS <- gsub("[？]", "?", textsS)
textsS <- gsub("[а]", "a", textsS)
textsS <- gsub("[с]", "c", textsS)
textsS <- gsub("[е]", "e", textsS)
textsS <- gsub("[іī]", "i", textsS)
textsS <- gsub("[оοO]", "o", textsS)
textsS <- gsub("[Sѕ]", "s", textsS)
textsS <- gsub("[у]", "y", textsS)
# remove non-printable characters i.e. not [:alnum:], [:punct:] and [:space:]
system.time(textsS <- gsub("[^[:print:]]+", " ", textsS))
# merge words/characters separated by period, aprostrophe and hyphen
system.time(textsS <- gsub("([[:alpha:]])[.'-]([[:alpha:]])", "\\1\\2", textsS))
# merge digits separated by colon, period and comma
system.time(textsS <- gsub("([[:digit:]])[:.,]([[:digit:]])", "\\1\\2", textsS))
# split text by period, question mark and exclamation mark to get sentences
system.time(textsS <- unlist(strsplit(textsS, "[.?!]+")))
# remove punctuations
system.time(textsS <- gsub("[[:punct:]]+", " ", textsS))
# remove excessive whitespaces
system.time(textsS <- gsub(" +", " ", textsS))
# strip whitespace at beginning/end of sentences
system.time(textsS <- gsub("^ | $", "", textsS))
# convert all characters to lower case
system.time(textsS <- tolower(textsS))

###
# sampling
###

load("RData/texts.RData")
set.seed(0)
sampleIndex <- split(sample(seq_along(texts)), rep_len(1:32, length(texts)))
# textsS <- texts[sampleIndex[[1]]]
textsS <- llply(sampleIndex, function(index) texts[index])
rm(sampleIndex)

###
# ngram frequency
###
# NGramTokenizer default delimiters = " \\r\\n\\t.,;:'"()?!"

# ngram functions

getNgrams <- function(texts_list, NgramTokenizerFUN) {
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

# unigram

wordCount <- table(unlist(strsplit(textsS, " ")))
wordCount <- sort(wordCount, decreasing = T)
wordCount <- wordCount[!grepl("[[:digit:]]", names(wordCount))]
wordcloud(names(wordCount), wordCount, scale = c(4,1), max.words = 200, 
          colors = brewer.pal(9, "Set1"))

unigram.df <- data.frame(word = names(wordCount), count = wordCount)

wordCountNum <- rev(table(wordCount))
wordProp <- data.frame(threshold = as.numeric(names(wordCountNum)),
                       count = wordCountNum,
                       uniqueProp = cumsum(wordCountNum) / sum(wordCountNum))
wordProp <- mutate(wordProp, 
                   totalCount = threshold * count,
                   totalProp = cumsum(totalCount) / sum(totalCount))
rm(wordCountNum)

qplot(uniqueProp, totalProp, data = subset(wordprop, uniqueProp <= 0.2),
      geom = "line",
      ylim = c(0,1))

# bigram

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
system.time(bigrams <- getNgrams(textsS, 2))
system.time(bigramCount <- getNgramCount(bigrams))
rm(bigrams)
system.time(bigramProp <- getNgramProp(bigramCount))
system.time(bigram.df <- getNgramDF(bigramCount))
system.time(bigram.df <- reduceNgramDF(bigram.df))
# save(bigram.df, file = "RData/bigramdf.RData")
# write.table(bigram.df, file = "RData/bigramdf.txt", row.names = F)

# unigram 2

unigramCount <- sort(table(bigram.df$suggestion), decreasing = T)
unigram.df <- data.frame(suggestion = names(unigramCount), count = unigramCount)
# save(unigram.df, file = "RData/unigramdf.RData")

# trigram

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
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
# prediction
###

load("RData/bigramdf.RData")
load("RData/trigramdf.RData")

cleanText <- function(textInput) {
    # remove non-printable characters i.e. not [:alnum:], [:punct:] and [:space:]
    textInput <- gsub("[^[:print:]]+", " ", textInput)
    # merge words/characters separated by period, aprostrophe and hyphen
    textInput <- gsub("([[:alpha:]])[.'-]([[:alpha:]])", "\\1\\2", textInput)
    # merge digits separated by punctuation and space
    textInput <- gsub("([[:digit:]])[[:punct:] ]([[:digit:]])", "\\1\\2", textInput)
    # replace ampersand with "and"
    textInput <- gsub("&+", " and ", textInput)
    # remove punctuations
    textInput <- gsub("[[:punct:]]+", " ", textInput)
    # remove excessive whitespaces
    textInput <- gsub(" +", " ", textInput)
    # strip whitespace at beginning/end of sentences
    textInput <- gsub("^ | $", "", textInput)
    # convert all characters to lower case
    textInput <- tolower(textInput)
    
    wordsOut <- strsplit(textInput, " ")
    return(wordsOut)
}

getNWords <- function(wordsIn, n) paste(tail(wordsIn, n), collapse = " ")

rankSuggestions <- function(wordsIn, suggestionIn, ngramdf) {
    wordsdf <- subset(ngramdf, words == wordsIn, c(suggestion, count))
    wordsdf <- mutate(wordsdf, prop = count / sum(count))
    wordsdf <- wordsdf[match(suggestionIn, wordsdf$suggestion),]
    wordsdf <- arrange(wordsdf, desc(count))
    return(wordsdf)
}

suggestWords <- function(wordsInput, npred, ngramdf) {
    wordsdf <- subset(ngramdf, words == wordsInput, c(suggestion, count))
    wordsdf <- mutate(wordsdf, prop = count / sum(count))
    wordsdf <- arrange(wordsdf, desc(count))
    return(head(wordsdf, npred))
}

chooseSuggestions <- function(suggestdf) {
    suggestdf <- ddply(suggestdf, .(suggestion), summarise, 
                       suggestion = head(suggestion, 1),
                       prop = max(prop))
    suggestdf <- head(arrange(suggestdf, desc(prop)), 3)
    return(suggestdf$suggestion)
}

###
# milestone report
###

knit2html("capstone-milestone.Rmd")

###
# quiz 2
###

rankSuggestions("case of", c("cheese", "soda", "pretzels", "beer"), trigram.df)
rankSuggestions("of", c("cheese", "soda", "pretzels", "beer"), bigram.df)

rankSuggestions("mean the", c("most", "best", "world", "universe"), trigram.df)
rankSuggestions("the", c("most", "best", "world", "universe"), bigram.df)

rankSuggestions("me the", c("happiest", "bluest", "saddest", "smelliest"), trigram.df)
rankSuggestions("the", c("happiest", "bluest", "saddest", "smelliest"), bigram.df)

rankSuggestions("but the", c("defense", "referees", "players", "crowd"), trigram.df)
rankSuggestions("the", c("defense", "referees", "players", "crowd"), bigram.df)

rankSuggestions("at the", c("groceries", "mall", "movies", "beach"), trigram.df)
rankSuggestions("the", c("groceries", "mall", "movies", "beach"), bigram.df)

rankSuggestions("on my", c("way", "motorcycle", "horse", "phone"), trigram.df)
rankSuggestions("my", c("way", "motorcycle", "horse", "phone"), bigram.df)

rankSuggestions("quite some", c("time", "thing", "years", "weeks"), trigram.df)
rankSuggestions("some", c("time", "thing", "years", "weeks"), bigram.df)

rankSuggestions("his little", c("fingers", "ears", "toes", "eyes"), trigram.df)
rankSuggestions("little", c("fingers", "ears", "toes", "eyes"), bigram.df)

rankSuggestions("during the", c("hard", "sad", "bad", "worse"), trigram.df)
rankSuggestions("the", c("hard", "sad", "bad", "worse"), bigram.df)

rankSuggestions("must be", c("callous", "asleep", "insane", "insensitive"), trigram.df)
rankSuggestions("be", c("callous", "asleep", "insane", "insensitive"), bigram.df)

###
# quiz 3
###

rankSuggestions("and id", c("eat", "die", "give", "sleep"), trigram.df)
rankSuggestions("id", c("eat", "die", "give", "sleep"), bigram.df)

rankSuggestions("about his", c("marital", "financial", "spiritual", "horicultural"), trigram.df)
rankSuggestions("his", c("marital", "financial", "spiritual", "horicultural"), bigram.df)

rankSuggestions("monkeys this", c("weekend", "month", "morning", "decade"), trigram.df)
rankSuggestions("this", c("weekend", "month", "morning", "decade"), bigram.df)

rankSuggestions("reduce your", c("happiness", "stress", "sleepiness", "hunger"), trigram.df)
rankSuggestions("your", c("happiness", "stress", "sleepiness", "hunger"), bigram.df)

rankSuggestions("take a", c("walk", "look", "picture", "minute"), trigram.df)
rankSuggestions("a", c("walk", "look", "picture", "minute"), bigram.df)

rankSuggestions("settle the", c("account", "incident", "case", "matter"), trigram.df)
rankSuggestions("the", c("account", "incident", "case", "matter"), bigram.df)

rankSuggestions("in each", c("toe", "arm", "finger", "hand"), trigram.df)
rankSuggestions("each", c("toe", "arm", "finger", "hand"), bigram.df)

rankSuggestions("to the", c("center", "side", "top", "middle"), trigram.df)
rankSuggestions("the", c("center", "side", "top", "middle"), bigram.df)

rankSuggestions("from playing", c("weekly", "inside", "daily", "outside"), trigram.df)
rankSuggestions("playing", c("weekly", "inside", "daily", "outside"), bigram.df)

rankSuggestions("adam sandlers", c("movies", "stories", "novels", "pictures"), trigram.df)
rankSuggestions("sandlers", c("movies", "stories", "novels", "pictures"), bigram.df)

suggestWords("the", 3, bigram.df)

stopCluster(cl)