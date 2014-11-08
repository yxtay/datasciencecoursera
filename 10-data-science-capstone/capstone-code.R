setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

###
# load packages
###

library(plyr); library(ggplot2); library(reshape2); library(gridExtra)
library(tm); library(SnowballC); library(qdap); library(RWeka)
library(wordcloud); library(RColorBrewer)
library(knitr); library(beepr)
# beep(2)

###
# read text
###

name <- c("blogs", "news", "twitter")
files <- paste0("./final/en_US/en_US.", name, ".txt")
system.time(texts <- llply(files, readLines))
names(texts) <- name

###
# Quiz 1
###

charCount <- llply(texts, nchar)
llply(charCount, max)
length(grep("love", texts[[3]]))/length(grep("hate", texts[[3]]))
grep("biostats", texts[[3]], value = T)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", texts[[3]]))

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
# samepling
###

set.seed(0)
lineCount <- llply(texts, length)
textsS <- llply(seq_along(texts), function(i) sample(texts[[i]], ceiling(0.01 * lineCount[[i]])))
textsS <- do.call(c, textsS)

###
# cleaning
###

system.time(textsS <- sent_detect(paste0(textsS, "|")))
system.time(textsS <- removePunctuation(textsS, preserve_intra_word_dashes = T))
system.time(textsS <- stripWhitespace(textsS))
system.time(textsS <- tolower(textsS))

###
# term frequency
###

system.time(wordCount <- sort(termFreq(PlainTextDocument(textsS)), decreasing = T))
head(which(cumsum(wordCount) > 0.8 * sum(wordCount)), 1)
head(which(cumsum(wordCount) > 0.5 * sum(wordCount)), 1)
df.words <- data.frame(word = names(wordCount), count = wordCount)
wordcloud(names(wordCount), wordCount, scale = c(5,1), max.words = 200, 
          colors = brewer.pal(9, "Set1"))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
system.time(bigramCount <- sort(termFreq(PlainTextDocument(textsS), control = list(tokenize = BigramTokenizer)),
                                decreasing = T))
bigramCount <- bigramCount[bigramCount > 1]
wordcloud(names(bigramCount), bigramCount, max.words = 200, 
          colors = brewer.pal(9, "Set1"))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
system.time(trigramCount <- sort(termFreq(PlainTextDocument(textsS), control = list(tokenize = TrigramTokenizer)),
                                 decreasing = T))
trigramCount <- trigramCount[trigramCount > 1]
wordcloud(names(trigramCount), trigramCount, scale = c(3, 0.5), max.words = 100, 
          colors = brewer.pal(9, "Set1"))

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
system.time(quadgramCount <- sort(termFreq(PlainTextDocument(textsS), control = list(tokenize = QuadgramTokenizer)),
                                  decreasing = T))
quadgramCount <- quadgramCount[quadgramCount > 1]

###
# milestone report
###

knit2html("capstone-milestone.Rmd")

###
# quiz 2
###

###
# quiz 3
###

###
# term document matrix
###

# tdm <- TermDocumentMatrix(PlainTextDocument(textsS))

# newsCorpus <- Corpus(VectorSource(texts[[2]]),
#                      readerControl = list(reader = readPlain))
# x <- tm_map(newsCorpus, sent_detect)
# 
# twitterCorpus <- PCorpus(URISource("./final/en_US/en_US.twitter.txt"),
#                          readerControl = list(reader = readPlain),
#                          dbControl = list(dbName = 'TwitterCorpus'))
# twitterCorpus <- tm_map(twitterCorpus, strsplit, split = "\n")
# 
# textCorpus <- PCorpus(DirSource('./final/en_US'), 
#                     readerControl = list(reader = readPlain),
#                     dbControl = list(dbName = 'CapstoneCorpus'))
