library(shiny)
source("shiny-stub.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
  
  #run({x <- shinyStub(input)})
  
  output$lifeHist <- renderPlot({
    hist(rnorm(1000, mean=75, sd=10), main="", sub="", xlab="Age (years)", freq=FALSE)
  })
  
  output$lifeExpect <- renderUI({
    fluidRow(
      "Life Expectancy : ",
      strong("75"),
      tags$small("(72,78)")
    )
  })
  
  output$qualAdjLifeExpect <- renderUI({
    fluidRow(
      "Quality Adjusted Life Expectancy : ",
      strong("72"),
      tags$small("(70, 74)")
    )
  })
  
  output$totalCosts <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("43,482")
    )
  })
  
  output$costEffectiveRatio <- renderUI({
    fluidRow(
      "Cost Effectiveness Ratio : ",
      strong("0.92"),
      tags$small("(0.90, 0.94)")
    )
  })
  
  output$stoppedTreat <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("161")
    )
  })
  
  output$stoppedTreat <- renderUI({
    fluidRow(
      "Stopped Treatement : ",
      strong("72")
    )
  })
  
  output$switchTreat <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong("4000")
    )
  })
  
  output$mldMyo <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("140")
    )
  })

  output$modMyo <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("3")
    )
  })
  
  output$sevMyo <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("1")
    )
  })
  
})
