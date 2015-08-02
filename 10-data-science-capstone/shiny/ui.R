library(shiny)

shinyUI(
    
    pageWithSidebar(
        
        headerPanel("Data Science Capstone Project"),
        
        column(4,
               
               wellPanel(
                   
                   h4("Corpora Weights"),
                   
                   sliderInput("blogs.w", "Blogs",
                               min = 0, max = 10, value = 1, step = 0.1),
                   
                   sliderInput("news.w", "News",
                               min = 0, max = 10, value = 1, step = 0.1),
                   
                   sliderInput("twitter.w", "Twitter",
                               min = 0, max = 10, value = 1, step = 0.1)
               ),
               
               wellPanel(
                   
                   h4("Ngram Weights"),
                   
                   sliderInput("ng.exp", "Exponent on ngram size",
                               min = -1, max = 4, value = 2, step = 0.1)
               )
               
        ),
        
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("App",
                         h4("Welcome to my word prediction app based on the ngram language model."),
                         p("Please type in the text box below."),
                         
                         textInput("text", "Type here", value = "Hello World!"),
                         
                         radioButtons("choice", "Predictions",
                                      choices = c("." = ""),
                                      inline = T),
                         
                         strong("Input Text"),
                         
                         verbatimTextOutput("textInput"),
                         
                         strong("Processed Text"),
                         
                         verbatimTextOutput("textProcessed"),
                         
                         strong("Predictions Table"),
                         
                         tableOutput("pred.tbl")
                ),
                
                tabPanel("Weights",
                         
                         plotOutput("cpw.plot"),
                         
                         plotOutput("ngw.plot")
                         ),
                
                tabPanel("Information",
                         
                         h3("Prediction Model"),
                         
                         p("The word prediction app is based on the ngram language model on the",
                           a("HC Corpura.", href = "http://www.corpora.heliohost.org/"),
                           "The HC Corpura is a collection of corpura collected from publicly available sources.",
                           "Only the English language corpura are used, of which there are",
                           "blogs, news and Twitter sources.",
                           "From the HC Corpura, ngrams are generated up to length of 5,",
                           "after some preprocessing, such as sentence detection and punctuation removal.",
                           "The ngrams are then counted and normalised into proportion figures.",
                           "Some smoothing is then applied to the proportion from the ngrams of different sizes.",
                           "Finally, the proportions from the 3 different corpora are combined on some weights."),
                         p("The user is able to control the corpora weights",
                           "and the amount of smoothing between ngram sizes."),
                         
                         
                         strong("Corpura Weights"),
                         
                         p("The relative weights of the 3 different corpura can be adjusted",
                           "based on the preference on the sources.",
                           "For example, contribution from a particular corpus, say Twitter,",
                           "can be removed by setting its weight to 0.",
                           "The weights are normalised so that they sum to 1 before being applied to the model."),
                         
                         p("At initiation, the 3 corpura are equally weighted."),
                         
                         strong("Ngram Weights"),
                         
                         p("The smoothing between ngrams of difference sizes is based on",
                           "the ngram size, n, raised to a certain power.",
                           "The exponent to which n is raised to can be adjusted by the slider.",
                           "A larger exponent places emphasis on larger ngrams,",
                           "which is naturally the preference.",
                           "Smaller ngrams can be given higher weights by setting the exponent to a negative value"),
                         
                         p("At initiation, the exponent is set to 2 to give larger ngrams more weights.")
                )
            )
        )
    )
)