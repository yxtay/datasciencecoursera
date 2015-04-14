library(plyr)

###
# prediction
###

load("RData/unigramdf.RData")
load("RData/bigramdf2.RData")
load("RData/trigramdf2.RData")

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
    # remove non-alphnumeric characters
    textInput <- gsub("[^[:alnum:]]+", " ", textInput)
    # remove excessive whitespaces
    textInput <- gsub(" +", " ", textInput)
    # strip whitespace at beginning/end of sentences
    textInput <- gsub("^ | $", "", textInput)
    # convert all characters to lower case
    textInput <- tolower(textInput)
    # convert all numbers to "<NUM>"
    textInput <- gsub("[[:digit:]]+", "<NUM>", textInput)
    
    wordsOut <- unlist(strsplit(textInput, " "))
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
    wordsdf <- subset(ngramdf, words == wordsInput, c(suggestion, prop))
    wordsdf <- arrange(wordsdf, desc(prop))
    return(head(wordsdf, npred))
}

chooseSuggestions <- function(suggestdf, nsuggest) {
    suggestdf <- ddply(suggestdf, .(suggestion), summarise, 
                       suggestion = head(suggestion, 1),
                       prop = max(prop))
    suggestdf <- head(arrange(suggestdf, desc(prop)), nsuggest)
    return(suggestdf$suggestion)
}
