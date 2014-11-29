setwd("~/GitHub/datasciencecoursera/10-data-science-capstone")

library(plyr)

###
# prediction
###

load("RData/unigramdf.RData")
load("RData/bigramdf2.RData")
load("RData/trigramdf2.RData")

cleanText <- function(textInput) {
    # remove non-printable characters i.e. not [:alnum:], [:punct:] and [:space:]
    textsS <- gsub("[^[:print:]]+", " ", textsS)
    # merge words/characters separated by period, aprostrophe and hyphen
    textsS <- gsub("([[:alpha:]])[.'-]([[:alpha:]])", "\\1\\2", textsS)
    # merge digits separated by punctuation and space
    textsS <- gsub("([[:digit:]])[[:punct:] ]([[:digit:]])", "\\1\\2", textsS)
    # replace ampersand with "and"
    textsS <- gsub("&+", " and ", textsS)
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
