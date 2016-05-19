library(shiny)
library(ggplot2)
source("simvastatin.R")
source("costs.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
  aPG   <- simvastatin(input)
  cPG   <- costs(aPG)
#  input$vPGx <- "None"
#  aNoPG <- simvastatin(input)
#  cNoPG <- costs(aNoPG)
  
  output$lifeHist <- renderPlot({
    x <- data.frame(Age=c(cPG$stats[,"QALY"], rep(NA, 14000)),
                    Treatment=c(rep("Genotyped",14000), rep("No Genotyping",14000)))
    ggplot(x, aes(Age, fill = Treatment)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
  })
  
  output$lifeExpect <- renderUI({
    fluidRow(
      "Life Expectancy : ",
      strong(as.character(round(cPG$boot$t0["Life"],1)+40)),
      tags$small(paste("(",
        as.character(round(cPG$life95$basic[4],1)+40),
        "-",
        as.character(round(cPG$life95$basic[5],1)+40),
        ")",sep=''))
    )
  })
  
  output$qualAdjLifeExpectNoPG <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong("XX"),
      tags$small("(XX-XX)")
    )
  })
  
  output$totalCostsNoPG <- renderUI({
    fluidRow(
      "Total Costs : $",
      strong("XX,XXX"),
      tags$small("($XX,XXX-$XX,XXX)")
    )
  })
  
  output$costEffectiveRatioNoPG <- renderUI({
    fluidRow(
      "Cost Effectiveness Ratio : ",
      strong("X.XX"),
      tags$small("(X.XX-X.XX)")
    )
  })
  
  output$qualAdjLifeExpectPG <- renderUI({
    fluidRow(
      "Quality Adjusted Life Exp : ",
      strong(as.character(round(cPG$boot$t0['QALY'],1)+40)),
      tags$small(paste("(",
        as.character(round(cPG$qaly95$basic[4],1)+40),
        "-",
        as.character(round(cPG$qaly95$basic[5],1)+40),
        ")",sep=''))
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
      strong("X.XX"),
      tags$small("(X.XX-X.XX)")
    )
  })
  
  output$ICER <- renderUI({
    fluidRow(
      "ICER : ",
      strong("X.XX"),
      tags$small("(X.XX-X.XX)")
    )
  })
  
  # Counts
  cnts <- table(aPG$resource)
  
  output$deathCVDPG <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong(as.character(cnts["cvd_death"]))
    )
  })
  
  output$stoppedTreatPG <- renderUI({
    fluidRow(
      "Stopped Treatment : ",
      strong(as.character(cnts["stopped"]))
    )
  })
  
  output$switchTreatPG <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong(as.character(cnts["switched"]))
    )
  })
  
  output$mldMyoPG <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong(as.character(cnts["mild_myopathy"]))
    )
  })

  output$modMyoPG <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong(as.character(cnts["mod_myopathy"]))
    )
  })
  
  output$sevMyoPG <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong(as.character(cnts["sev_myopathy"]))
    )
  })
  
  
  output$deathCVDNoPG <- renderUI({
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong("X")
    )
  })

  output$mldMyoNoPG <- renderUI({
    fluidRow(
      "Mild Myopathies : ",
      strong("X")
    )
  })
  
  output$modMyoNoPG <- renderUI({
    fluidRow(
      "Moderate Myopathies : ",
      strong("X")
    )
  })
  
  output$sevMyoNoPG <- renderUI({
    fluidRow(
      "Severe Myopathies : ",
      strong("X")
    )
  })
  
  output$stoppedTreatNoPG <- renderUI({
    fluidRow(
      "Stopped Treatement : ",
      strong("X")
    )
  })
  
  output$switchTreatNoPG <- renderUI({
    fluidRow(
      "Switched Treatment : ",
      strong("X")
    )
  })
  
})
