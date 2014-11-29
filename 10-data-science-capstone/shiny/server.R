library(shiny)
library(plyr)

load("RData/unigramdf.RData")
load("RData/bigramdf2.RData")
load("RData/trigramdf2.RData")
# load("RData/quadgramdf2.RData")

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

shinyServer(function(input, output) {
    
    output$textIn <- renderText( input$text )
    
    output$textOut <- renderText( cleanText(input$text) )
    
    output$value <- renderText({
        textIn <- cleanText(input$text)
        bigramSuggest <- suggestWords(tail(textIn, 1), 3, bigram.df2)
        trigramSuggest <- suggestWords(getNWords(textIn, 2), 3, trigram.df2)
#         quadgramSuggest <- suggestWords(getNWords(textIn, 2), 3, quadgram.df2)
        suggestion <- chooseSuggestions(rbind(bigramSuggest, trigramSuggest), 1)
        if (length(suggestion) < 1) suggestion <- head(unigram.df$suggestion, 1)
        suggestion
    })
    
})