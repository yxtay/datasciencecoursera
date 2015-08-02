library(shiny)

shinyUI(
    
    pageWithSidebar(
        
        headerPanel("Data Science Capstone Project"),
        
        column(4,
               
               wellPanel(
                   
                   h4("Corpus Weights"),
                   
                   sliderInput("blogs.w", "Blogs",
                               min = 0, max = 10, value = 1, step = 0.1),
                   
                   sliderInput("news.w", "News",
                               min = 0, max = 10, value = 1, step = 0.1),
                   
                   sliderInput("twitter.w", "Twitter",
                               min = 0, max = 10, value = 1, step = 0.1)
               ),
               
               wellPanel(
                   
                   h4("N-gram Weights"),
                   
                   sliderInput("ng.exp", "Exponent on n-gram size",
                               min = -1, max = 4, value = 2, step = 0.1)
               ),
               
               p(strong("Slide Deck:"),
                 a("RPubs", href = "http://rpubs.com/yxtay/dsc-slides"))
               
        ),
        
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("App",
                         h4("Welcome to my word prediction app based on the n-gram language model."),
                         
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
                         
                         p("The word prediction app is based on the n-gram language model on the",
                           a("HC Corpora.", href = "http://www.corpora.heliohost.org/"),
                           "The HC Corpora is a collection of corpora collected from publicly available sources.",
                           "Only the English language corpora are used, of which there are",
                           "blogs, news and Twitter sources.",
                           "From the HC corpora, n-grams are generated up to length of 5,",
                           "after some preprocessing, such as sentence detection and punctuation removal.",
                           "The n-grams are then counted and normalised into proportion figures.",
                           "Some smoothing is then applied to the proportion from the n-grams of different sizes.",
                           "Finally, the proportions from the 3 different corpora are combined on some preset weights."),
                         p("The user is able to control the corpus weights",
                           "and the amount of smoothing between n-gram sizes."),
                         
                         
                         strong("Corpus Weights"),
                         
                         p("The relative weights of the 3 different corpora can be adjusted",
                           "based on the preference on the sources.",
                           "For example, contribution from a particular corpus, say Twitter,",
                           "can be removed by setting its weight to 0.",
                           "The weights are normalised so that they sum to 1 before being applied to the model."),
                         
                         p("At initiation, the 3 corpora are equally weighted."),
                         
                         strong("N-gram Weights"),
                         
                         p("The smoothing between n-grams of difference sizes is based on",
                           "the n-gram size, n, raised to a certain power.",
                           "The exponent to which n is raised to can be adjusted by the slider.",
                           "A larger exponent places emphasis on larger n-grams,",
                           "which is naturally the preference.",
                           "Smaller n-grams can be given higher weights by setting the exponent to a negative value"),
                         
                         p("At initiation, the exponent is set to 2 to give larger n-grams more weights.")
                )
            )
        )
    )
)