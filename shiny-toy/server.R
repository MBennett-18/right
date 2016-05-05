library(shiny)

source("shiny-life.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$distPlot <- renderPlot({

    x    <- shinyLife(input$drisk)

    # draw the histogram with the specified number of bins
    par(mfrow=c(1,2))
    hist(x["strokes",], col = 'darkgray', border = 'white', main="Strokes", freq=FALSE, xlab="100 runs of 50 patients", breaks=8)
    abline(v = median(x["strokes",]), col="red", lwd=2)
    abline(v = mean(x["strokes",]), col="blue", lwd=2)
    hist(x["deaths",], col = 'darkgray', border = 'white', main="Deaths", freq=FALSE, xlab="", breaks=seq(-0.5, 10.5, 1))
    abline(v = median(x["deaths",]), col="red", lwd=2)
    abline(v = mean(x["deaths",]), col="blue", lwd=2)
  })
})
