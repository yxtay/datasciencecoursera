library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Data Science Capstone Project"),
    
    p("Welcome to my ngram language model prediction algorithm. Please type in your message into the text box below.
      The app may take some time to initialise, please be patient."),
    
    # Copy the line below to make a text input box
    textInput("text", label = h3("Type here:"), value = "Hello World!"),
    
    h3("Input Text"),
    
    fluidRow(verbatimTextOutput("textIn")),
    
    h3("Processed Text"),
    
    fluidRow(verbatimTextOutput("textOut")),
    
    h3("Suggestion"),
    
    fluidRow(verbatimTextOutput("value"))
))