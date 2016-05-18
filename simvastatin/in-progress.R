source("simvastatin.R")

##############################################
##
## Piddly bits for development
##
##
##
# For testing right now
inputs      <- list()
inputs$vAge <- 40
inputs$vTX  <- TRUE
inputs$vPGx <- "Reactive"
inputs$vSecondLine <- "Atorvastin"
#inputs$vSecondLine <- "Rosuvastatin"
#inputs$vSecondLine <-  "Low/Mod Dose Statin"
inputs$vCostDrug1 <- 147
inputs$vCostDrug2 <- 173.1
inputs$vCostDrug3 <- 259.2
inputs$vCostDrug4 <- 143.7
inputs$vCostPGx   <- 250

N <- 14000

library(dplyr)

traj <- simulation(env, inputs)

# Run the simulation 
env %>% create_counters(counters) %>%
  add_generator("patient", traj, at(rep(0, N)), mon=2) %>%
  run(36500) %>% # Simulate 100 years.
  wrap()

# Look at summary statistics
arrivals <- get_mon_arrivals(env, per_resource = T)

#####################################################
# Costing Algorithm
#

annual_discount_rate <- 0.03
cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale

discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
}

# Look at summary statistics
arrivals <- get_mon_arrivals(env, per_resource = T)

# Fix up event times

# Treat all events as 1 day, for computation of cost
events_to_fix_end <- c("mild_myopathy", "mod_myopathy","sev_myopathy", "cvd", "genotyped")
arrivals[arrivals$resource %in% events_to_fix_end,]$end_time <- arrivals[arrivals$resource %in% events_to_fix_end,]$start_time + 1.0

# Compute total activity times
arrivals$activity_time <- arrivals$end_time - arrivals$start_time

# Computes discounted rate of time
arrivals$discounted_time <- discounted_cost(arrivals$start_time, arrivals$end_time, 365.0)

# Compute Event base costs
arrivals$base_cost <- sapply(arrivals$resource, FUN=function(resource){
  if(     resource == "drug1")          inputs$vCostDrug1/365
  else if(resource == "drug2")          inputs$vCostDrug2/365
  else if(resource == "drug3")          inputs$vCostDrug3/365
  else if(resource == "drug4")          inputs$vCostDrug4/365
  else if(resource == "genotyped")      inputs$vCostPGx
  else if(resource == "mild_myopathy")  129
  else if(resource == "mod_myopathy")  2255
  else if(resource == "sev_myopathy") 12811
  else if(resource == "cvd")          20347
  else 0
})

# Computed discounted costs
arrivals$discounted_cost <- arrivals$base_cost*arrivals$discounted_time

# Patient costs
costs <- aggregate(arrivals$discounted_cost, by=list(name=arrivals$name), simplify=TRUE, FUN=sum)
