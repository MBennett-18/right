
source("simvastatin.R")
source("costs.R")


##############################################
##
## Piddly bits for development
##
##
##
# For testing right now

input <- list(
  vAge        = 40,
  vTX         = TRUE,
  vSecondLine = "Atorvastin",
  vCostDrug1  = 147,
  vCostDrug2  = 173.1,
  vCostDrug3  = 259.2,
  vCostDrug4  = 143.7,
  vCostPGx    = 250,
  vPGx        = "Prospective"
)

aPG   <- simvastatin(input)
cPG   <- costs(aPG)

length(aPG[aPG$resource=="life",]$name)


input$vPGx <- NA
aNoPG <- simvastatin(input)
cNoPG <- costs(aNoPG)

length(aNoPG[aNoPG$resource=="life",]$name)
