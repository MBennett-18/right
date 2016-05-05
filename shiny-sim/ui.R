library(shiny)

shinyUI(fluidPage(
  titlePanel("Cost Effectiveness of Pharmacogenomic Testing for Simvastatin"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  sidebarLayout(
    sidebarPanel(
        submitButton("Run Simulation"),
        checkboxInput("vPGx", "Perform Prospective Genotypic Testing", 1),
        checkboxInput("vTx",  "Prescribe Treatment", 1),
        selectInput("vSecondLine", "Second Line Drug",
                    c("Atorvastin", "Rosuvastatin", "Low/Mod Dose Statin"), "Low/Mod Dose Statin", FALSE),
        wellPanel(
          h3("Costs in Dollars"),
          sliderInput("vCostPGx",   "Genotyping (one time)", min=10, max=750, value=250, step=1),
          sliderInput("vCostDrug1", "Simvastatin (yearly)", min=1.0, max=2000, value=147, step=0.1),
          sliderInput("vCostDrug2", "Atorvastin (yearly)", min=1.0, max=2000, value=173.1, step=0.1),
          sliderInput("vCostDrug3", "Rosuvastatin (yearly)", min=1.0, max=2000, value=259.2, step=0.1),
          sliderInput("vCostDrug4", "Low/Mod Dose Statin (yearly)", min=1.0, max=2000, value=143.7, step=0.1)
        ),
        wellPanel(
          h3("Population"),
          p("TODO: Currently a fixed age. Will be modified soon to reflect actual population distribution at Vanderbilt"),
          sliderInput("vAge", "Starting Age", min = 30, max=70, value=40, step=1)
        )
    ),

    mainPanel(
      h2("Simulation Results"),
      htmlOutput("lifeExpect"),
      plotOutput("lifeHist"),
      
      h3("Costs"),
      htmlOutput("qualAdjLifeExpect"),
      htmlOutput("costEffectiveRatio"),
      htmlOutput("totalCosts"),
      
      h3("Counts"),
      htmlOutput("deathCVD"),
      htmlOutput("stoppedTreat"),
      htmlOutput("switchTreat"),
      htmlOutput("mldMyo"),
      htmlOutput("modMyo"),
      htmlOutput("sevMyo"),
      
      h3("Simulation Method"),
      img(src="SimvastatinMethod.png", width="60%")
    )
  ),
  p("Built using ",
        a(href="https://www.r-project.org/", "R"),
        "with the ",
        a(href="https://cran.r-project.org/web/packages/simmer/index.html", "simmer"),
        " and ",
        a(href="https://cran.r-project.org/web/packages/shiny/shiny.pdf", "shiny"),
        "packages."
  ),
  p("TODO: Website footer here")
))
