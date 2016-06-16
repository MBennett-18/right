library(shiny)

shinyUI(fluidPage(
  titlePanel("Cost Effectiveness of Pharmacogenomic Testing for Simvastatin"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  sidebarLayout(
    sidebarPanel(
        submitButton("Run Simulation"),
        
        wellPanel(
          h3("Testing Strategy"),
          selectInput("vPreemptive", "Preemptive Genotyping",
                      c("None", "Panel", "PREDICT", "Age >= 50"), "None", FALSE),
          selectInput("vReactive", "Reactive Genotyping",
                      c("None", "Single Condition", "Panel"), "None", FALSE),
          checkboxGroupInput("vPanel", "Panel Genes",
                      c("Simvastatin (slco1b1)",
                        "Warfarin (vkorc1, cyp2c9)",
                        "Cloidogrel (cyp2c19)"),
                      selected= c("Simvastatin (slco1b1)",
                                  "Warfarin (vkorc1, cyp2c9)",
                                  "Cloidogrel (cyp2c19)")),
          sliderInput("vPREDICTsens", "PREDICT Sensitivity", min=0, max=1, value=0.05, step=0.01),
          sliderInput("vPREDICTspec", "PREDICT Specificity", min=0, max=1, value=0.05, step=0.01)
        ),
        
        checkboxInput("smooth", "Smooth", TRUE),
        conditionalPanel(
          condition = "console.log(input) && input.smooth",
          selectInput("smoothMethod", "Method",
                      list("lm", "glm", "gam", "loess", "rlm"))
        ),
        wellPanel(
           h3("Population"),
           selectInput("vN", "Simulated Patients",
                        c("1000", "10,000", "100,000"),
                        "10,000", FALSE),
            p("Patient's starting age is drawn from a uniform distribuion"),
            numericInput("vLowerAge", "Lowest Age", min=20, max=80, value=40, step=1),
            sliderInput("vUpperAge", "Upper Age",  min=20, max=80, value=40, step=1),
            sliderInput("vHorizon",  "Time Horizon", min=1, max=80, value=10, step=1)
        ),
        wellPanel(
          h3("Population Genetics"),
          h4("Clopidogrel"),
          sliderInput("vClopidogrelMedRisk", "Medium Adverse Risk %", min=0, max=1, value=0.23, step=0.001),
          sliderInput("vClopidogrelHighRisk", "High Adverse Risk %", min=0, max=1, value=0.23, step=0.001),
          h4("Simvastatin"),
          sliderInput("vSimvastatinMedRisk", "Medium Adverse Risk %", min=0, max=1, value=0.249, step=0.001),
          sliderInput("vSimvastatinHighRisk", "High Adverse Risk %", min=0, max=1, value=0.021, step=0.001),
          h4("Warfarin"),
          sliderInput("vWarfarinMedRisk",   "Medium Adverse Risk %", min=0, max=1, value=0.23, step=0.001),
          sliderInput("vWarfarinHighRisk",  "High Adverse Risk %", min=0, max=1, value=0.23, step=0.001)
        ),
        wellPanel(
          h3("Simvastatin Adverse Events"),

          h4("Mild Myopathy"),
          sliderInput("vMildMyoBaseNoVar",   "Base Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoBaseMedVar",  "Base Risk | Medium Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoBaseHighVar", "Base Risk | High Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoDrugNoVar",   "Simvastatin Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoDrugMedVar",  "Simvastatin Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoDrugHighVar", "Simvastatin Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoAltNoVar",    "Alternate Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoAltMedVar",   "Alternate Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vMildMyoAltHighVar",  "Alternate Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001),
          
          h4("Moderate Myopathy"),
          sliderInput("vModMyoNoVar",        "Base Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoMedVar",       "Base Risk | Medium Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoHighVar",      "Base Risk | High Variant", value=0.5,min=0, max=10, step=0.001),
          sliderInput("vModMyoDrugNoVar",    "Simvastatin Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoDrugMedVar",   "Simvastatin Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoDrugHighVar",  "Simvastatin Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoAltNoVar",     "Alternate Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoAltMedVar",    "Alternate Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vModMyoAltHighVar",   "Alternate Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001),
          
          h4("Severe Myopathy"),
          sliderInput("vSevMyoNoVar",        "Base Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoVar",          "Base Risk | Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoDrugNoVar",    "Simvastatin Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoDrugMedVar",   "Simvastatin Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoDrugHighVar",  "Simvastatin Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoAltNoVar",     "Alternate Rel. Risk | No Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoAltMedVar",    "Alternate Rel. Risk | Medium Risk Variant", value=0.5, min=0, max=10, step=0.001),
          sliderInput("vSevMyoAltHighVar",   "Alternate Rel. Risk | High Risk Variant", value=0.5, min=0, max=10, step=0.001)
          
# Switching strategy ???
          
        ),

        wellPanel(
          h3("Costs in Dollars"),
          sliderInput("vCostPanelGene",  "Genotyping (panel)", min=10, max=750, value=250, step=1),
          sliderInput("vCostSingleGene", "Genotyping (single)", min=10, max=750, value=250, step=1),
          h4("Simvastatin"),
          sliderInput("vCostDrug1", "Simvastatin (yearly)", min=0.0, max=2000, value=147, step=0.1),
          sliderInput("vCostDrug2", "Second Line (yearly)", min=0.0, max=2000, value=173.1, step=0.1)
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
