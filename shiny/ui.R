library(shiny)

shinyUI(fluidPage(
  titlePanel("ARENA Stroke Example"),

  sidebarLayout(
    sidebarPanel(
        sliderInput("drisk",
                    "Death Risk during stroke:",
                    min=0.02,
                    max=0.34,
                    value=0.19,
                    step=0.01)
    ),

    mainPanel(
      plotOutput("distPlot")
    )
  )
))
