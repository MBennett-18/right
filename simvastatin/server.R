library(shiny)
library(ggplot2)
source("simvastatin.R")
source("costs.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
  aPG   <- simvastatin(input)
  cPG   <- costs(aPG)
  input$vPGx <- "None"
  aNoPG <- simvastatin(input)
  cNoPG <- costs(aNoPG)
  
  output$lifeHist <- renderPlot({
    x <- data.frame(Age=c(aPG$QALY),
                    Treatment=c(rep("Prescribed",1000), rep("No Treatment",1000)))
    ggplot(x, aes(Age, fill = Treatment)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
  })
  
  output$lifeExpect <- renderUI({
    fluidRow(
      "Life Expectancy : ",
      strong("75"),
      tags$small("(72-78)")
    )
  })
  
  output$qualAdjLifeExpectNoPG <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong("72"),
      tags$small("(70-74)")
    )
  })
  
  output$totalCostsNoPG <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("43,482"),
      tags$small("($41,200-$45,432)")
    )
  })
  
  output$costEffectiveRatioNoPG <- renderUI({
    fluidRow(
      "Cost Effectiveness Ratio : ",
      strong("0.92"),
      tags$small("(0.90-0.94)")
    )
  })
  
  output$qualAdjLifeExpectPG <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong("72"),
      tags$small("(70-74)")
    )
  })
  
  output$totalCostsPG <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("43,482"),
      tags$small("($41,200-$45,432)")
    )
  })
  
  output$costEffectiveRatioPG <- renderUI({
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
  output$deathCVDPG <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("161")
    )
  })
  
  output$stoppedTreatPG <- renderUI({
    fluidRow(
      "Stopped Treatement : ",
      strong("72")
    )
  })
  
  output$switchTreatPG <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong("4000")
    )
  })
  
  output$mldMyoPG <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("140")
    )
  })

  output$modMyoPG <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("3")
    )
  })
  
  output$sevMyoPG <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("1")
    )
  })
  
  
  output$deathCVDNoPG <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("450")
    )
  })

  output$mldMyoNoPG <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("400")
    )
  })
  
  output$modMyoNoPG <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("12")
    )
  })
  
  output$sevMyoNoPG <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("3")
    )
  })
  
  output$stoppedTreatNoPG <- renderUI({
    fluidRow(
      "Stopped Treatement : ",
      strong("0")
    )
  })
  
  output$switchTreatNoPG <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong("0")
    )
  })
  
})
