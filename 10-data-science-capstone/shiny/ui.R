library(shiny)

shinyUI(fluidPage(
    
    # Copy the line below to make a text input box
    textInput("text", label = h3("Text input"), value = "Hello World!"),
    
    hr(),
    
    h3("Input"),
    
    fluidRow(verbatimTextOutput("textIn")),
    
    h3("Suggestion"),
    
    fluidRow(verbatimTextOutput("value"))
))