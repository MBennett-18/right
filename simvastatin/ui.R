library(shiny)


parameterInput <- function(inputId, label, value, min=NA, max=NA, step=NA, width=NULL)
{
  fluidRow(column(width=7, HTML(label)),
           column(width=5, numericInput(inputId, NA, value, min, max, step)))
  #parameterInput(inputId, label, value, min, max, step, "60%")
}



shinyUI(fluidPage(
  titlePanel("Cost Effectiveness of Pharmacogenomic Testing for Simvastatin"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  sidebarLayout(
    sidebarPanel(
        submitButton("Run Simulation"),
        h3("Parameters"),
        tabsetPanel(type="tabs",
          tabPanel(
              "Population",
               selectInput("vN", "Simulated Patients",
                                  c("1000", "10,000", "100,000"),
                                  "10,000", FALSE),
               p("Patient's starting age is drawn from a uniform distribuion"),
               parameterInput("vLowerAge", "Lowest Age", min=20, max=80, value=40, step=1),
               parameterInput("vUpperAge", "Upper Age",  min=20, max=80, value=40, step=1),
               parameterInput("vHorizon",  "Time Horizon", min=1, max=80, value=10, step=1)
          ),
                    
        tabPanel(
          "Testing Strategy",
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
          parameterInput("vPREDICTsens", "PREDICT Sensitivity", min=0, max=1, value=0.05, step=0.01),
          parameterInput("vPREDICTspec", "PREDICT Specificity", min=0, max=1, value=0.05, step=0.01)
        ),
        tabPanel(
          "Genetics",
          h4("Clopidogrel"),
          parameterInput("vClopidogrelMedRisk",  "Med Adverse Risk %",  0.23, 0, 1, 0.001),
          parameterInput("vClopidogrelHighRisk", "High Adverse Risk %", 0.23, 0, 1, 0.001),
          h4("Simvastatin"),
          parameterInput("vSimvastatinMedRisk",  "Med Adverse Risk %",  0.23, 0, 1, 0.001),
          parameterInput("vSimvastatinHighRisk", "High Adverse Risk %", 0.23, 0, 1, 0.001),
          h4("Warfarin"),
          parameterInput("vWarfarinMedRisk",     "Med Adverse Risk %",  0.23, 0, 1, 0.001),
          parameterInput("vWarfarinHighRisk",    "High Adverse Risk %", 0.23, 0, 1, 0.001)
        ),
        tabPanel(
          "Simvastatin",

          h3("Mild Myopathy"),
          h4("Base Risk"),
          parameterInput("vMildMyoBaseNoVar",   "Low Adverse",    0.5, 0, 10, 0.001),
          parameterInput("vMildMyoBaseMedVar",  "Medium Adverse", 0.5, 0, 10, 0.001),
          parameterInput("vMildMyoBaseHighVar", "High Adverse",   0.5, 0, 10, 0.001),
          h4("Simvastatin Relative Risk"),
          parameterInput("vMildMyoDrugNoVar",   "Low Adverse",    0.5, 0, 10, 0.001),
          parameterInput("vMildMyoDrugMedVar",  "Medium Adverse", 0.5, 0, 10, 0.001),
          parameterInput("vMildMyoDrugHighVar", "High Adverse",   0.5, 0, 10, 0.001),
          h4("Alternate Relative Risk"),
          parameterInput("vMildMyoAltNoVar",    "Low Adverse",    0.5, 0, 10, 0.001),
          parameterInput("vMildMyoAltMedVar",   "Medium Adverse", 0.5, 0, 10, 0.001),
          parameterInput("vMildMyoAltHighVar",  "High Adverse",   0.5, 0, 10, 0.001),

          h3("Moderate Myopathy"),
          h4("Base Risk"),
          parameterInput("vModMyoBaseNoVar",   "Low Adverse",     0.5, 0, 10, 0.001),
          parameterInput("vModMyoBaseMedVar",  "Medium Adverse",  0.5, 0, 10, 0.001),
          parameterInput("vModMyoBaseHighVar", "High Adverse",    0.5, 0, 10, 0.001),
          h4("Simvastatin Relative Risk"),
          parameterInput("vModMyoDrugNoVar",   "Low Adverse",     0.5, 0, 10, 0.001),
          parameterInput("vModMyoDrugMedVar",  "Medium Adverse",  0.5, 0, 10, 0.001),
          parameterInput("vModMyoDrugHighVar", "High Adverse",    0.5, 0, 10, 0.001),
          h4("Alternate Relative Risk"),
          parameterInput("vModMyoAltNoVar",    "Low Adverse",     0.5, 0, 10, 0.001),
          parameterInput("vModMyoAltMedVar",   "Medium Adverse",  0.5, 0, 10, 0.001),
          parameterInput("vModMyoAltHighVar",  "High Adverse",    0.5, 0, 10, 0.001),

          h3("Severe Myopathy"),
          h4("Base Risk"),
          parameterInput("vSevMyoBaseNoVar",   "Low Adverse",     0.5, 0, 10, 0.001),
          parameterInput("vSevMyoBaseMedVar",  "Medium Adverse",  0.5, 0, 10, 0.001),
          parameterInput("vSevMyoBaseHighVar", "High Adverse",    0.5, 0, 10, 0.001),
          h4("Simvastatin Relative Risk"),
          parameterInput("vSevMyoDrugNoVar",   "Low Adverse",     0.5, 0, 10, 0.001),
          parameterInput("vSevMyoDrugMedVar",  "Medium Adverse",  0.5, 0, 10, 0.001),
          parameterInput("vSevMyoDrugHighVar", "High Adverse",    0.5, 0, 10, 0.001),
          h4("Alternate Relative Risk"),
          parameterInput("vSevMyoAltNoVar",   "Low Adverse",      0.5, 0, 10, 0.001),
          parameterInput("vSevMyoAltMedVar",  "Medium Adverse",   0.5, 0, 10, 0.001),
          parameterInput("vSevMyoAltHighVar", "High Adverse",     0.5, 0, 10, 0.001)

# Switching strategy ???
          
        ),

        tabPanel(
          "Costs in Dollars",
          parameterInput("vCostPanelGene",  "Genotyping (panel)", min=10, max=750, value=250, step=1),
          parameterInput("vCostSingleGene", "Genotyping (single)", min=10, max=750, value=250, step=1),
          h4("Simvastatin"),
          parameterInput("vCostDrug1", "Simvastatin (yearly)", min=0.0, max=2000, value=147, step=0.1),
          parameterInput("vCostDrug2", "Second Line (yearly)", min=0.0, max=2000, value=173.1, step=0.1)
        )
    )),

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
