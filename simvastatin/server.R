library(shiny)
library(ggplot2)
source("simvastatin.R")
source("costs.R")

deci <- function(x, k) format(round(x, k), nsmall=k)

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
  results <- reactive({
    inputs <- list(vAge = input$vAge,
                   vTX  = input$vTX,
                   vSecondLine = input$vSecondLine,
                   vCostDrug1  = input$vCostDrug1,
                   vCostDrug2  = input$vCostDrug2,
                   vCostDrug3  = input$vCostDrug3,
                   vCostDrug4  = input$vCostDrug4,
                   vCostPGx    = input$vCostPGx,
                   vPGx        = input$vPGx)
    
    aPG <- simvastatin(inputs) 
    cPG <- costs(aPG)
    
    # Counts
    cntsPG <- table(aPG$resource)
    
    #  input$vPGx <- "None"
    #  aNoPG <- simvastatin(input)
    #  cNoPG <- costs(aNoPG)
    
    list(aPG=aPG,cPG=cPG,cntsPG=cntsPG)
  })
  
  output$lifeHist <- renderPlot({
    cPG <- results()[['cPG']]

    x <- data.frame(Age=c(cPG[,"QALY"], rep(NA, 14000)),
                    Treatment=c(rep("Genotyped",14000), rep("No Genotyping",14000)))
    ggplot(x, aes(Age, fill = Treatment)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
  })
  
  output$lifeExpect <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats[,"Life"])
    
    fluidRow(
      "Time in Simulation Life Expectancy : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
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
    stats <- results()[['cPG']]
    quant <- quantile(stats[,"QALY"])
    
    fluidRow(
      "Quality Adjusted Life Exp : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$totalCostsPG <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats[,"Discounted Cost"])
    
    fluidRow(
      "Total Disc Costs : $",
      tags$small(deci(quant[2],2)),
      strong(deci(quant[3],2)),
      tags$small(deci(quant[4],2))
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
  
  output$deathCVDPG <- renderUI({
    cnts <- results()[['cntsPG']]
    
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong(as.character(cnts["cvd_death"]))
    )
  })
  
  output$stoppedTreatPG <- renderUI({
    cnts <- results()[['cntsPG']]
    fluidRow(
      "Stopped Treatment : ",
      strong(as.character(cnts["stopped"]))
    )
  })
  
  output$switchTreatPG <- renderUI({
    cnts <- results()[['cntsPG']]
    fluidRow(
      "Switched Treatment : ",
      strong(as.character(cnts["switched"]))
    )
  })
  
  output$mldMyoPG <- renderUI({
    cnts <- results()[['cntsPG']]
    fluidRow(
      "Mild Myopathies : ",
      strong(as.character(cnts["mild_myopathy"]))
    )
  })

  output$modMyoPG <- renderUI({
    cnts <- results()[['cntsPG']]
    fluidRow(
      "Moderate Myopathies : ",
      strong(as.character(cnts["mod_myopathy"]))
    )
  })
  
  output$sevMyoPG <- renderUI({
    cnts <- results()[['cntsPG']]
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
