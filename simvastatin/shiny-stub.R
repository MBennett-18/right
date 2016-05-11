library(shiny)
library(simmer)
library(dplyr)

shinyStub <- function(input)
{
  stub <- round(c(81+(input$vPGx*10), 64, 55000, 22, 64, 81, 41, 161, 70, 14000, 4000, 140, 3, 3, 1400, 4300) + rnorm(16))
  data.frame(
    Measure=c("Life Expectancy", 
              "CVD Death Age",
              "Cost",
              "Quality",
              "Time to CVD",
              "Age at Death",
              "Death Time",
              "Death by CVD Count",
              "Stopped Treatment Count",
              "Natural Deaths Count", 
              "Switched Treatment Count",
              "Mild Myopathy Count",
              "Moderate Myopathy Count",
              "Severe Myopathy Count",
              "CVD Count",
              "Low Dose Patients"),
    Mean=stub,
    Lower95CI=round(stub-abs(rnorm(16, sd=sqrt(stub))), 2),
    Upper95CI=round(stub+abs(rnorm(16, sd=sqrt(stub))), 2)
   ) 
}
