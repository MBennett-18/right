library(shiny)

shinyUI(fluidPage(
  titlePanel("Cost Effectiveness of Pharmacogenomic Testing for Simvastatin"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  sidebarLayout(
    sidebarPanel(
        submitButton("Run Simulation"),
        selectInput("vPGx", "Testing Method",
                    c("Prospective", "Reactive"), "Prospective", FALSE),
        selectInput("vSecondLine", "Second Line Drug",
                    c("Atorvastin", "Rosuvastatin", "Low/Mod Dose Statin"), "Atorvastin", FALSE),
        wellPanel(
          h3("Probabilities"),
          sliderInput("vDiscRate", "Discontinuation Rate", min=0.0, max=1.0, value=0.03, step=0.01)
        ),
        wellPanel(
          h3("Costs in Dollars"),
          sliderInput("vCostPGx",   "Genotyping (one time)", min=10, max=750, value=250, step=1),
          sliderInput("vCostDrug1", "Simvastatin (yearly)", min=0.0, max=2000, value=147, step=0.1),
          sliderInput("vCostDrug2", "Atorvastin (yearly)", min=0.0, max=2000, value=173.1, step=0.1),
          sliderInput("vCostDrug3", "Rosuvastatin (yearly)", min=0.0, max=2000, value=259.2, step=0.1),
          sliderInput("vCostDrug4", "Low/Mod Dose Statin (yearly)", min=0.0, max=2000, value=143.7, step=0.1)
        ),
        wellPanel(
          h3("Population"),
          p("TODO: Currently a fixed age. Will be modified soon to reflect actual population distribution at Vanderbilt"),
          sliderInput("vAge", "Starting Age", min = 30, max=70, value=40, step=1)
        )
    ),

    mainPanel(
      h2("Simulation Results"),
      p("Numbers are given in ",tags$small("25%")," ", strong("50%")," ",tags$small("75%")," quantiles"),
      htmlOutput("lifeExpect"),
      h2(""),
      plotOutput("lifeHist"),
      p("Quality Adjusted Life In Sim Histogram"),
      
      h3("Costs"),
      splitLayout(
        fluidPage(
          h4("No Testing"),
          htmlOutput("qualAdjLifeExpectNoPG"),
          htmlOutput("costEffectiveRatioNoPG"),
          htmlOutput("totalCostsNoPG")
        ),
        fluidPage(
          h4("With Genotyping"),
          htmlOutput("qualAdjLifeExpectPG"),
          htmlOutput("costEffectiveRatioPG"),
          htmlOutput("totalCostsPG"),
          htmlOutput("ICER")
        )
      ),
      
      h3("Counts"),
      splitLayout(
        fluidPage(
          h4("No Testing"),
          htmlOutput("deathCVDNoPG"),
          htmlOutput("mldMyoNoPG"),
          htmlOutput("modMyoNoPG"),
          htmlOutput("sevMyoNoPG"),
          htmlOutput("stoppedTreatNoPG"),
          htmlOutput("switchTreatNoPG")
        ),
        fluidPage(
          h4("With Genotyping"),
          htmlOutput("deathCVDPG"),
          htmlOutput("mldMyoPG"),
          htmlOutput("modMyoPG"),
          htmlOutput("sevMyoPG"),
          htmlOutput("stoppedTreatPG"),
          htmlOutput("switchTreatPG")
        )
      ),

      h3("Simulation Method"),
      img(src="SimvastatinMethod.png", width="60%")
    )
  ),
  p("Built using ",
        a(href="https://www.r-project.org/", img(src="Rlogo.png", alt="R", width="60")),
        "with the ",
        a(href="https://cran.r-project.org/web/packages/simmer/index.html", "simmer"),
        " and ",
        a(href="https://cran.r-project.org/web/packages/shiny/shiny.pdf", "shiny"),
        "packages."
  ),
  p("TODO: Website footer here")
))
