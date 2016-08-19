
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

input$vTX <- FALSE
aNoTX <- simvastatin(input)
cNoTX <- costs(aNoTX)

x <- data.frame(Age=c(cNoPG$QALY, cNoTX$QALY),
                Treatment=c(rep("Treated",N), rep("No Treatment",N)))
ggplot(x, aes(Age, fill = Treatment)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
