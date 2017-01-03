library(shiny)


parameterInput <- function(inputId, label, value, min=NA, max=NA, step=NA, width=NULL)
{
  fluidRow(column(width=5, HTML(label)),
           column(width=7, numericInput(inputId, NA, value, min, max, step)))
  #parameterInput(inputId, label, value, min, max, step, "60%")
}



shinyUI(fluidPage(
  titlePanel("Cost Effectiveness of Pharmacogenomic Testing for Simvastatin"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  sidebarLayout(
    sidebarPanel(width=6,
        submitButton("Run Simulation"),
        h3("Parameters"),
        tabsetPanel(type="tabs",
          tabPanel(
              "Population",
               selectInput("vN", "Simulated Patients",
                                  c("1000", "10,000", "50,000", "100,000"),
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
          parameterInput("vPREDICTsens", "PREDICT Sensitivity", min=0, max=1, value=0.3, step=0.01),
          parameterInput("vPREDICTspec", "PREDICT Specificity", min=0, max=1, value=0.3, step=0.01)
        ),
        tabPanel(
          "Genetics",
          h3("Phenotypic Prevalence"),
          h4("Clopidogrel"),
          parameterInput("vClopidogrelMedMet",  "Med Metabolizers %",  0.23, 0, 1, 0.001),
          parameterInput("vClopidogrelPoorMet", "Poor Metabolizers %", 0.23, 0, 1, 0.001),
          h4("Simvastatin"),
          parameterInput("vSimvastatinMedMet",  "Med Metabolizers %",  0.249, 0, 1, 0.001),
          parameterInput("vSimvastatinPoorMet", "Poor Metabolizers %", 0.021, 0, 1, 0.001),
          h4("Warfarin"),
          parameterInput("vWarfarinMedMet",     "Med Metabolizers %",  0.23, 0, 1, 0.001),
          parameterInput("vWarfarinPoorMet",    "Poor Metabolizers %", 0.23, 0, 1, 0.001)
        ),
        tabPanel(
          "Simvastatin",
        
          h3("Physician Behavior"),
            
          parameterInput("vProbSimvastatinAlt", "Prob. of Alt | Variant", 1.00, 0, 1, 0.001),
          parameterInput("vProbSimStopMild","Prob. of Stop | Mild Myo",   0.23, 0, 1, 0.001),
          parameterInput("vProbSimStopMod", "Prob. of Stop | Mod Myo",    0.23, 0, 1, 0.001),
          parameterInput("vProbSimStopSev", "Prob. of Stop | Sev Myo",    1.00, 0, 1, 0.001),
          
          p(em("Note: Events greater than a year are dropped from simulation.")),
          p(em("Med and Poor refer to phenotypic metabolizer status.")),

          h3("Mild Myopathy"),
          h4("No Drug Risk"),
          parameterInput("vMildMyoBaseNoVar",  "Baseline Risk", 1e-7,    0, 100, 0.001),
          
          h4("Simvastatin Risk"),
          parameterInput("vMildMyoSimNoVar",   "Baseline Risk", 0.05,    0, 100, 0.001),
          parameterInput("vMildMyoSimMedVar",  "Rel Risk|Med",  1,       0, 100, 0.001),
          parameterInput("vMildMyoSimPoorVar", "Rel Risk|Poor", 1,       0, 100, 0.001),
          h4("Alternate Risk"),
          parameterInput("vMildMyoAltNoVar",   "Baseline Risk", 0.05,    0, 100, 0.001),
          parameterInput("vMildMyoAltMedVar",  "Rel Risk|Med",  1,       0, 100, 0.001),
          parameterInput("vMildMyoAltPoorVar", "Rel Risk|Poor", 1,       0, 100, 0.001),

          h3("Moderate Myopathy"),
          h4("No Drug Risk"),
          parameterInput("vModMyoBaseNoVar",   "Baseline Risk",  1e-10,   0, 100, 0.001),
          h4("Simvastatin Risk"),
          parameterInput("vModMyoSimNoVar",    "Baseline Risk",  0.00011, 0, 100, 0.001),
          parameterInput("vModMyoSimMedVar",   "Rel Risk|Med",   2.55,    0, 100, 0.001),
          parameterInput("vModMyoSimPoorVar",  "Rel Risk|Poor",  9.56,    0, 100, 0.001),
          h4("Alternate Risk"),
          parameterInput("vModMyoAltNoVar",    "Baseline Risk",  0.00011, 0, 100, 0.001),
          parameterInput("vModMyoAltMedVar",   "Rel Risk|Med",   1.08,    0, 100, 0.001),
          parameterInput("vModMyoAltPoorVar",  "Rel Risk|Poor",  4.05,    0, 100, 0.001),

          h3("Severe Myopathy"),
          h4("No Drug Risk"),
          parameterInput("vSevMyoBaseNoVar",   "Baseline Risk",  1e-16,   0, 100, 0.001),
          h4("Simvastatin Risk"),
          parameterInput("vSevMyoSimNoVar",    "Baseline Risk",  0.000034,0, 100, 0.001),
          parameterInput("vSevMyoSimMedVar",   "Rel Risk|Med",   2.55,    0, 100, 0.001),
          parameterInput("vSevMyoSimPoorVar",  "Rel Risk|Poor",  9.56,    0, 100, 0.001),
          h4("Alternate Risk"),
          parameterInput("vSevMyoAltNoVar",    "Baseline Risk",  0.000034,0, 100, 0.001),
          parameterInput("vSevMyoAltMedVar",   "Rel Risk|Med",   1.08,    0, 100, 0.001),
          parameterInput("vSevMyoAltPoorVar",  "Rel Risk|Poor",  4.05,    0, 100, 0.001)

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

    mainPanel(width=6,
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
