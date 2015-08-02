library(shiny)

source("prediction.R")

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