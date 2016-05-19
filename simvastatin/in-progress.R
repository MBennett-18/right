
source("simvastatin.R")
source("costs.R")

##############################################
##
## Piddly bits for development
##
##
##
# For testing right now

inputs <- list(
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

#arrivals <- simvastatin(inputs)
#length(arrivals[arrivals$resource=="life",]$name)

aPG   <- simvastatin(inputs)
cPG   <- costs(aPG)

inputs$vPGx <- NA
aNoPG <- simvastatin(inputs)
cNoPG <- costs(aNoPG)
