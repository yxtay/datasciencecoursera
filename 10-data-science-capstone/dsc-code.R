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

# shiny app upload

setwd("./shiny/")
deployApp(appName = "dsc-shiny")

# r pubs upload

write("options(rpubs.upload.method = \"internal\")", ".Rprofile")
options(rpubs.upload.method = "internal")
