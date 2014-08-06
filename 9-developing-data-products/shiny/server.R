library(shiny)

shinyServer(function(input, output) {
    
    output$plot <- renderPlot({
        with(cars, {
            plot(speed, dist, pch = 20, cex = 2,
                 main = "Stopping Distance of Cars against Speed",
                 xlab = "Speed (mph)", ylab = "Stopping distance (ft)")
            lines(ksmooth(speed, dist, kernel = input$kern1, bandwidth = input$bw1), 
                  col = 2, lwd = 2)
            lines(ksmooth(speed, dist, kernel = input$kern2, bandwidth = input$bw2), 
                  col = 3, lwd = 2)
            points(ksmooth(speed, dist, kernel = input$kern1, bandwidth = input$bw1,
                           x.point = input$speed),
                   col = 2, cex = 2, pch = 18)
            points(ksmooth(speed, dist, kernel = input$kern2, bandwidth = input$bw2,
                           x.point = input$speed), 
                   col = 3, cex = 2, pch = 18)
            legend("topleft", legend = c("Smoother 1", "Smoother 2"), col = 2:3, lwd = 2, bty = "n")
        })
    })
    
    output$speed <- renderText(input$speed)
    
    output$dist1 <- renderText({
        with(cars, {
            dist1 <- ksmooth(speed, dist, kernel = input$kern1, bandwidth = input$bw1,
                             x.point = input$speed)$y
            sprintf("%.2f", dist1)
        })
    })
    
    output$dist2 <- renderText({
        with(cars, {
            dist2 <- ksmooth(speed, dist, kernel = input$kern2, bandwidth = input$bw2,
                             x.point = input$speed)$y
            sprintf("%.2f", dist2)
        })
    })
})