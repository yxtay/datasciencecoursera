library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Tuning Parameters for the Nadaraya-Watson Kernel Regression Estimator"),
    
    sidebarPanel(
        sliderInput("bw1", "Bandwidth of Smoother 1:",
                    min = 2, max = 10, value = 2, step = 0.5,
                    animate = animationOptions(interval = 1500, loop = T)),
        selectInput('kern1', 'Kernel of Smoother 1:', choices = c("normal", "box")),
        
        sliderInput("bw2", "Bandwidth of Smoother 2",
                    min = 2, max = 10, value = 8, step = 0.5,
                    animate = animationOptions(interval = 1500, loop = T)),
        selectInput('kern2', 'Kernel of Smoother 2:', choices = c("normal", "box")),
        
        numericInput("speed", "Speed of car", value = 18, min = 4, max = 25),
        
        h4("For input speed:"),
        verbatimTextOutput("speed"),
        
        h4("Stopping distance from Smoother 1:"),
        verbatimTextOutput("dist1"),
        
        h4("Stopping distance from Smoother 2:"),
        verbatimTextOutput("dist2")
    ),
    
    mainPanel(
        h4("Instructions"),
        p("This app serves to demonstrate the Nadaraya-Watson (NW) kernel regression estimator 
          and its tuning parameters using the cars dataset."),
        p("The NW estimators are plotted together with data points from the cars data. 
          There are 2 tuning parameters for the NW estimator: bandwidth and kernel. 
          The user can tweak them to observe how they affect the estimators.
          The play button for the bandwidth setting allows the user to
          observe how the estimator changes as bandwidth increases.
          2 NW estimators are plotted to allow the user to compare
          the estimators with different tuning parameters set."),
        p("The final input is used to predict the stopping distance of the car
          with the given speed. The predictions for both estimators are printed
          on the side panel."),
        
        plotOutput("plot")
    )
    
))