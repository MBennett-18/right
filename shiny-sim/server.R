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
      tags$small("(72-78)")
    )
  })
  
  output$qualAdjLifeExpectNoTX <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong("72"),
      tags$small("(70-74)")
    )
  })
  
  output$totalCostsNoTX <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("43,482"),
      tags$small("($41,200-$45,432)")
    )
  })
  
  output$costEffectiveRatioNoTX <- renderUI({
    fluidRow(
      "Cost Effectiveness Ratio : ",
      strong("0.92"),
      tags$small("(0.90-0.94)")
    )
  })
  
  output$qualAdjLifeExpectTX <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong("72"),
      tags$small("(70-74)")
    )
  })
  
  output$totalCostsTX <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("43,482"),
      tags$small("($41,200-$45,432)")
    )
  })
  
  output$costEffectiveRatioTX <- renderUI({
    fluidRow(
      "Cost Effectiveness Ratio : ",
      strong("0.92"),
      tags$small("(0.90-0.94)")
    )
  })
  
  output$ICER <- renderUI({
    fluidRow(
      "ICER : ",
      strong("0.92"),
      tags$small("(0.90-0.94)")
    )
  })
  
  # Counts
  output$deathCVDTX <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("161")
    )
  })
  
  output$stoppedTreatTX <- renderUI({
    fluidRow(
      "Stopped Treatement : ",
      strong("72")
    )
  })
  
  output$switchTreatTX <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong("4000")
    )
  })
  
  output$mldMyoTX <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("140")
    )
  })

  output$modMyoTX <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("3")
    )
  })
  
  output$sevMyoTX <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("1")
    )
  })
  
  
  output$deathCVDNoTX <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("450")
    )
  })

  output$mldMyoNoTX <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("400")
    )
  })
  
  output$modMyoNoTX <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("12")
    )
  })
  
  output$sevMyoNoTX <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("3")
    )
  })
  
})
