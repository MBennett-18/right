library(shiny)
library(ggplot2)
source("simvastatin.R")
source("costs.R")

deci <- function(x, k) format(round(x, k), nsmall=k)

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
  N <- 5000
  
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
    
    aPG <- simvastatin(inputs, N) 
    cPG <- costs(aPG)
    
    # Counts
    cntsPG <- table(aPG$resource)
    
    inputs$vPGx <- "None"
    aNoPG <- simvastatin(inputs, N)
    cNoPG <- costs(aNoPG)
    cntsNoPG <- table(aNoPG$resource)

    list(aPG=aPG,cPG=cPG,cntsPG=cntsPG,
         aNoPG=aNoPG,cNoPG=cNoPG,cntsNoPG=cntsNoPG)
  })
  
  output$lifeHist <- renderPlot({
    r <- results()
    cPG <- r[['cPG']]
    cNoPG <- r[['cNoPG']]

    x <- data.frame(Age=c(cPG$QALY, cNoPG$QALY),
                    Treatment=c(rep("Genotyped",N), rep("No Genotyping",N)))
    ggplot(x, aes(Age, fill = Treatment)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
  })
  
  output$lifeExpect <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats$Life)
    
    fluidRow(
      "Time in Simulation Life Expectancy : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$qualAdjLifeExpectNoPG <- renderUI({
    stats <- results()[['cNoPG']]
    quant <- quantile(stats$QALY)
    
    fluidRow(
      "Quality Adjusted Life Exp : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$totalCostsNoPG <- renderUI({
    stats <- results()[['cNoPG']]
    quant <- quantile(stats$Discount.Cost)
    
    fluidRow(
      "Total Disc Costs : $",
      tags$small(deci(quant[2],0)),
      strong(deci(quant[3],0)),
      tags$small(deci(quant[4],0))
    )
  })
  
  output$costEffectiveRatioNoPG <- renderUI({
    stats <- results()[['cNoPG']]
    quant <- quantile(stats$ce_ratio)
    
    fluidRow(
      "Cost Eff Ratio : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$qualAdjLifeExpectPG <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats$QALY)
    
    fluidRow(
      "Quality Adjusted Life Exp : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$totalCostsPG <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats$Discount.Cost)
    
    fluidRow(
      "Total Disc Costs : $",
      tags$small(deci(quant[2],0)),
      strong(deci(quant[3],0)),
      tags$small(deci(quant[4],0))
    )
  })
  
  output$costEffectiveRatioPG <- renderUI({
    stats <- results()[['cPG']]
    quant <- quantile(stats$ce_ratio)
    
    fluidRow(
      "Cost Eff Ratio : ",
      tags$small(deci(quant[2],1)),
      strong(deci(quant[3],1)),
      tags$small(deci(quant[4],1))
    )
  })
  
  output$ICER <- renderUI({
    r <- results()
    cPG   <- r[['cPG']]
    cNoPG <- r[['cNoPG']]

    icer <- (median(cNoPG$Discount.Cost) - median(cPG$Discount.Cost)) /
            (median(cNoPG$QALY)          - median(cPG$QALY))
    
    fluidRow(
      "ICER : ",
      strong(deci(icer,2))
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
    cnts <- results()[['cntsNoPG']]
    
    fluidRow(
      "Deaths by a Cardiovascular Event : ",
      strong(as.character(cnts["cvd_death"]))
    )
  })
  
  output$stoppedTreatNoPG <- renderUI({
    cnts <- results()[['cntsNoPG']]
    fluidRow(
      "Stopped Treatment : ",
      strong(as.character(cnts["stopped"]))
    )
  })
  
  output$switchTreatNoPG <- renderUI({
    cnts <- results()[['cntsNoPG']]
    fluidRow(
      "Switched Treatment : ",
      strong(as.character(cnts["switched"]))
    )
  })
  
  output$mldMyoNoPG <- renderUI({
    cnts <- results()[['cntsNoPG']]
    fluidRow(
      "Mild Myopathies : ",
      strong(as.character(cnts["mild_myopathy"]))
    )
  })
  
  output$modMyoNoPG <- renderUI({
    cnts <- results()[['cntsNoPG']]
    fluidRow(
      "Moderate Myopathies : ",
      strong(as.character(cnts["mod_myopathy"]))
    )
  })
  
  output$sevMyoNoPG <- renderUI({
    cnts <- results()[['cntsNoPG']]
    fluidRow(
      "Severe Myopathies : ",
      strong(as.character(cnts["sev_myopathy"]))
    )
  })

})
