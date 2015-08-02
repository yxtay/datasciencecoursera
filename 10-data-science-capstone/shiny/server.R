library(shiny)
library(data.table)
library(stringi)
library(ggplot2)
theme_set(theme_classic())

load("RData/prob_table.RData")
load("RData/word_index.RData")

process_input <- function(s) {
    # split into sentences
    s <- stri_split_boundaries(s, type = "sentence")
    s <- unlist(lapply(s, function(s) {
        # convert encoding to ascii
        s <- iconv(s, to = "ascii", sub = "")
        # convert to lower case
        s <- stri_trans_tolower(s)
        # remove punctuations
        s <- stri_replace_all_regex(s, "[[:punct:]]", "")
        # split into words
        s <- stri_split_boundaries(s, type = "word", skip_word_none = T)
        # pad with empty string at beginning of sentence
        s <- c("", s)
    }))
    # handle numbers
    s <- stri_replace_all_regex(s, "^[[:digit:]]+$", "<NUM>")
    s <- stri_replace_all_regex(s, "[[:digit:]]+", "")
}

input_table <- function(s, n = 4) {
    # obtain last n words from processed input
    s <- tail(s, n)
    # replace words with numbers using word.index
    s <- match(s, word.index, nomatch = match("<RARE>", word.index))
    # pad with NA is word count < n
    if (length(s) < n) {s <- c(rep(NA, n - length(s)), s)}
    # create data.table from processed input and name columns appropriately
    s.tbl <- as.data.table(as.list(s))
    setnames(s.tbl, 1:n, paste0("wordm", n:1))
    # create a few copies of the previous tables with first few words removed.
    s.tbl <- rbindlist(lapply(0:n, function(i) {
        if (i == 0) {return(copy(s.tbl))}
        copy(s.tbl)[, eval(names(s.tbl)[1:i]) := NA]
    }))
    unique(setkeyv(s.tbl, names(s.tbl)))
}

rank_pred <- function(s.tbl, prob.table, n.pred = 5, cp.weights = c(1, 1, 1), ng.weights = sqrt(1:5)) {
    # corpus weights to be referred by name
    names(cp.weights) <- c("blogs", "news", "twitter")
    # combine prediction probabilities from different corpus by cp.weights
    tbl <- prob.table[s.tbl, .(p = sum(p * cp.weights[corpus]) / sum(cp.weights)), 
                      by = c(key(prob.table), "n"), nomatch = 0]
    # obtain size of largest n-gram in predictions
    mn <- max(tbl[["n"]])
    if (is.null(mn)) mn <- 1
    # combine prediction probabilities from different n-gram size by ng.weights
    tbl <- tbl[, .(p = sum(p * ng.weights[n]) / sum(ng.weights[1:mn])), by = pred]
    # replace predictions with actual word and take the largest n.pred ones ordered by probabilities
    tbl[, `:=` (pred = word.index[pred])][order(-p)[1:min(.N, n.pred)]]
}

shinyServer(function(input, output, clientData, session) {
    # render unedited input text
    output$textInput <- renderText({ input$text })
    # process input text
    processed <- reactive({ process_input(input$text) })
    # render processed text
    output$textProcessed <- renderText({ paste0('"', processed(), '"', collapse = ", ") })
    # obtain data.table on processed input text
    input.tbl <- reactive({
        cp.weights <- c(input$blogs.w, input$news.w, input$twitter.w)
        ng.weights <- (1:5) ^ input$ng.exp
        s.tbl <- input_table(processed())
        rank_pred(s.tbl, prob.table, n.pred = 10, cp.weights = cp.weights, ng.weights = ng.weights)
        })
    # render prediction table with normalised score
    output$pred.tbl <- renderTable({
        pred.table <- copy(input.tbl())
        pred.table[, `:=` (s = p / sum(p))]
        setnames(pred.table[, c("pred", "s"), with = F], c("pred", "s"), c("Predicted Word", "Normalised Score"))
    })
    
    observe({
        # react to itself
        input$choice
        # obtain 5 predictions based on updated input text
        n = 5
        pred.tbl <- copy(input.tbl())
        pred <- head(pred.tbl[["pred"]], n)
        choices <- paste0(" ", pred)
        names(choices) <- paste(1:n, pred, sep = ". ")
        # update radio buttons with predictions
        updateRadioButtons(session,
                           "choice", "Predictions",
                           choices = c("." = "", as.list(choices)),
                           inline = T)
    })
    
    # update value of text input on choice selection
    observeEvent(input$choice,
                 
                 updateTextInput(session,
                                 "text", "Type here", value = paste0(input$text, input$choice))
    )
    
    # render plot of corpus weights after normalising weights
    output$cpw.plot <- renderPlot({
        cp.weights <- c(input$blogs.w, input$news.w, input$twitter.w)
        cp.weights <- cp.weights / sum(cp.weights)
        qplot(c("Blogs", "News", "Twitter"), cp.weights,
              geom = "bar", stat = "identity",
              main = "Corpus Weights",
              xlab = "", ylab = "",
              ylim = c(0, 1)) +
            geom_text(aes(label = round(cp.weights, 3)), hjust = 0.5, vjust = -0.5)
    })
    
    # render plot of n-gram weights after normalising weights
    output$ngw.plot <- renderPlot({
        ng.weights <- (1:5) ^ input$ng.exp
        ng.weights <- ng.weights / sum(ng.weights)
        qplot(1:5, ng.weights,
              geom = "bar", stat = "identity",
              main = "N-gram Weights",
              xlab = "", ylab = "",
              ylim = c(0, 1)) +
            geom_text(aes(label = round(ng.weights, 3)), hjust = 0.5, vjust = -0.5)
    })
})