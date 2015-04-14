setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

###
# load packages
###

library(plyr); library(reshape2)
library(ggplot2); library(gridExtra)
library(RWeka)
library(wordcloud); library(RColorBrewer)
library(knitr); library(beepr)
library(shinyapps)
# beep(2)

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
# preprocessing
###

# source("preprocess.R")

###
# ngram functions
###

source("ngram-functions.R")

###
# ngram tokenize
###

# source("ngram-tokenize.R")

###
# milestone report
###

knit2html("dsc-milestone.Rmd")

###
# prediction
###

load("RData/bigramdf.RData")
load("RData/trigramdf.RData")

source("prediction.R")

###
# quiz 2
###

choices <- c("cheese", "soda", "pretzels", "beer")
rankSuggestions("case of", choices, trigram.df)
rankSuggestions("of", choices, bigram.df)

choices <- c("most", "best", "world", "universe")
rankSuggestions("mean the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("happiest", "bluest", "saddest", "smelliest")
rankSuggestions("me the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("defense", "referees", "players", "crowd")
rankSuggestions("but the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("groceries", "mall", "movies", "beach")
rankSuggestions("at the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("way", "motorcycle", "horse", "phone")
rankSuggestions("on my", choices, trigram.df)
rankSuggestions("my", choices, bigram.df)

choices <- c("time", "thing", "years", "weeks")
rankSuggestions("quite some", choices, trigram.df)
rankSuggestions("some", choices, bigram.df)

choices <- c("fingers", "ears", "toes", "eyes")
rankSuggestions("his little", choices, trigram.df)
rankSuggestions("little", choices, bigram.df)

choices <- c("hard", "sad", "bad", "worse")
rankSuggestions("during the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("callous", "asleep", "insane", "insensitive")
rankSuggestions("must be", choices, trigram.df)
rankSuggestions("be", choices, bigram.df)

###
# quiz 3
###

choices <- c("eat", "die", "give", "sleep")
rankSuggestions("and id", choices, trigram.df)
rankSuggestions("id", chioces, bigram.df)

choices <- c("marital", "financial", "spiritual", "horicultural")
rankSuggestions("about his", choices, trigram.df)
rankSuggestions("his", choices, bigram.df)

choices <- c("weekend", "month", "morning", "decade")
rankSuggestions("monkeys this", choices, trigram.df)
rankSuggestions("this", choices, bigram.df)

choices <- c("happiness", "stress", "sleepiness", "hunger")
rankSuggestions("reduce your", choices, trigram.df)
rankSuggestions("your", choices, bigram.df)

choices <- c("walk", "look", "picture", "minute")
rankSuggestions("take a", choices, trigram.df)
rankSuggestions("a", choices, bigram.df)

choices <- c("account", "incident", "case", "matter")
rankSuggestions("settle the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("toe", "arm", "finger", "hand")
rankSuggestions("in each", choices, trigram.df)
rankSuggestions("each", choices, bigram.df)

choices <- c("center", "side", "top", "middle")
rankSuggestions("to the", choices, trigram.df)
rankSuggestions("the", choices, bigram.df)

choices <- c("weekly", "inside", "daily", "outside")
rankSuggestions("from playing", choices, trigram.df)
rankSuggestions("playing", choices, bigram.df)

choices <- c("movies", "stories", "novels", "pictures")
rankSuggestions("adam sandlers", choices, trigram.df)
rankSuggestions("sandlers", choices, bigram.df)

# shiny app upload

deployApp(appDir = paste0(getwd(),"/shiny"), appName = "dsc-shiny")

# r pubs upload

write("options(rpubs.upload.method = \"internal\")", ".Rprofile")
options(rpubs.upload.method = "internal")
