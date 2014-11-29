setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

library(plyr)

###
# load data
###

name <- c("blogs", "news", "twitter")
files <- paste0("./final/en_US/en_US.", name, ".txt")
texts <- llply(files, readLines, encoding = "UTF-8", skipNul = T)
names(texts) <- name
texts <- llply(texts, iconv, from = "UTF-8", to = "UTF-8", sub = "")
rm(name, files)

###
# preprocess data
###

textsS <- do.call(c, texts)
rm(texts)

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
textsS <- gsub("[^[:print:]]+", " ", textsS)
# merge words/characters separated by period, aprostrophe and hyphen
textsS <- gsub("([[:alpha:]])[.'-]([[:alpha:]])", "\\1\\2", textsS)
# merge digits separated by punctuation and space
textsS <- gsub("([[:digit:]])[[:punct:] ]([[:digit:]])", "\\1\\2", textsS)
# replace ampersand with "and"
textsS <- gsub("&+", " and ", textsS)
# split text by period, question mark and exclamation mark to get sentences
textsS <- unlist(strsplit(textsS, "[.?!]+"))
# remove punctuations
textsS <- gsub("[[:punct:]]+", " ", textsS)
# remove non-alphnumeric characters
textsS <- gsub("[^[:alnum:]]+", " ", textsS)
# remove excessive whitespaces
textsS <- gsub(" +", " ", textsS)
# strip whitespace at beginning/end of sentences
textsS <- gsub("^ | $", "", textsS)
# convert all characters to lower case
textsS <- tolower(textsS)
# convert all numbers to "<NUM>"
textsS <- gsub("[[:digit:]]+", "<NUM>", textsS)

###
# save preprocessed data
###

texts <- textsS[textsS != ""]
rm(textsS)
save(texts, file = "RData/texts.RData")
