---
output: html_document
---

# Capstone Milestone Report




```r
library(plyr); library(ggplot2); library(reshape2); library(gridExtra)
library(tm); library(SnowballC); library(qdap); library(RWeka)
library(wordcloud); library(RColorBrewer)
```

## Introduction

The purpose of the capstone project was to build a predictive text model from a large corpus of text documents 
using the skills learned in the Data Science Specialisation. 
New data science techniques of text analytics and natural language processing were required for the project. 
This report would explain the exploratory analysis the author has gone through 
to gain an understanding of the data given and outline the steps explored to achieve the goal of the project.

## Data


```r
name <- c("blogs", "news", "twitter")
files <- paste0("./final/en_US/en_US.", name, ".txt")
texts <- llply(files, readLines)
names(texts) <- name
```

The data consisted of text documents in 4 different languages: English, Finnish, German and Russian. 
In each language, text documents were obtain from 3 sources: blogs, news and Twitter. 
For the purpose of this project, only the English documents were analysed 
and of which the predictive text model would be built on. 
Brief information of the documents was provided in the table below.


```r
docSize <- sapply(texts, object.size)
lineCount <- sapply(texts, length)
charCount <- llply(texts, nchar)
meanChar <- sapply(charCount, mean)
(df.exp <- data.frame(lineCount, docSize, meanChar))
```

```
##         lineCount   docSize meanChar
## blogs      899288 260564320 231.6960
## news        77259  20111392 203.0024
## twitter   2360148 316037600  68.8029
```

Evidently, the documents contained varying amount of text data, with the Twitter document the largest 
and the news document the smallest, both in terms of number of lines and memory space taken. 
Specifically, the Twitter document contained 30.55 
times the number of lines and took 15.71 times 
the memory space compared to the news document. 
However, as expected, each line of the Twitter document contained less number of characters 
compared to the other 2 documents. 
Part of the reason was due to the 140 character limit restricted by Twitter.

## Exploratory Analysis

At this initial exploratory stage, no attempt was made to clean up the text documents 
to correct punctuations or spelling. 
The `strsplit()` function with argument `split = " "` was used directly to break the documents into individual words. 
The histogram of the word count per document by source type were shown below.


```r
words <- llply(texts, strsplit, split = " ")
wordCount <- llply(words, laply, length)
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
```

<img src="figure/wordCount-1.png" title="plot of chunk wordCount" alt="plot of chunk wordCount" style="display: block; margin: auto;" />

The histograms showed that the word count per document from each were distributed very differently. 
For blogs, the distribution was very skewed with a long right tail from long blog posts of over 200 words. 
However, there were quite a number with less than 15 words. 
For the news source, the distribution was less skewed with a mode number of words at around 30-40 per document. 
For Twitter, the documents had much fewer number of words as expected, with a mode of about 8 words.

## Building the Model

The language model would work with analysing ngrams from the documents. 
The documents would first need to be preprocessed by removing punctuations, 
striping excess whitespaces and converting to lower case.
The plan would be to then break down the documents into unigrams, bigrams and trigrams and 
count the frequencies of the ngrams. 
The frequency counts would then be normalised by the first (n-1) words ahead to form a probability vector. 
The probabilities would then be sorted in decreasing order, 
with the last words of the top 3 ngrams forming the suggestions for the app. 


```r
set.seed(0)
lineCount <- llply(texts, length)
textsS <- llply(seq_along(texts), function(i) sample(texts[[i]], ceiling(0.01 * lineCount[[i]])))
textsS <- do.call(c, textsS)

textsS <- sent_detect(paste0(textsS, "|"))
textsS <- removePunctuation(textsS, preserve_intra_word_dashes = T)
textsS <- stripWhitespace(textsS)
textsS <- tolower(textsS)
```

To employ all the information of the unigram, bigram and trigrams available, 
the trigrams would first be used to create suggestions. 
If first 2 words could not be matched or the suggested word had a low probability, 
the bigram would then be used for suggestion, followed by the unigram. 
The unigram would basically be individual words sorted by count.

The word clouds below showed the top couple of hundred ngrams for unigrams, 
bigrams and trigrams respectively for a 1% random sample of documents provided.

**Unigram word cloud**

```r
wordCount <- sort(termFreq(PlainTextDocument(textsS)), decreasing = T)
wordcloud(names(wordCount), wordCount, scale = c(5,1), max.words = 200, 
          colors = brewer.pal(9, "Set1"))
```

<img src="figure/unigram-1.png" title="plot of chunk unigram" alt="plot of chunk unigram" style="display: block; margin: auto;" />

**Bigram word cloud**

```r
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigramCount <- sort(termFreq(PlainTextDocument(textsS), control = list(tokenize = BigramTokenizer)),
                    decreasing = T)
wordcloud(names(bigramCount), bigramCount, max.words = 200, 
          colors = brewer.pal(9, "Set1"))
```

<img src="figure/bigram-1.png" title="plot of chunk bigram" alt="plot of chunk bigram" style="display: block; margin: auto;" />

**Trigram word cloud**

```r
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigramCount <- sort(termFreq(PlainTextDocument(textsS), control = list(tokenize = TrigramTokenizer)),
                     decreasing = T)
wordcloud(names(trigramCount), trigramCount, scale = c(3, 0.5), max.words = 100, 
          colors = brewer.pal(9, "Set1"))
```

<img src="figure/trigram-1.png" title="plot of chunk trigram" alt="plot of chunk trigram" style="display: block; margin: auto;" />
