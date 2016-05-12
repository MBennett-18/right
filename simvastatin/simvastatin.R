  ###########################################################################
 ##
## Health Policy Cost Effectiveness Simulation
##
## This is a modifiable simulator, that allows for estimation of costs and
## quality adjusted life years for patients undergoing various health events.
## 
## The event loop is abstracted. Each tick of the "now()" is a day. All
## internal units for the simulation are days.
##
## There is an event_registry where users can add events in a straight-
## forward manner. Some knowledge of simmer is still required, but the
## intricacies of the main event loop are dealt with.
##
## This leaves the top half of this example file as a modifiable
## section, and the bottom half as event loop code that hopefully 
## does not require modification. If it does, it should be folded
## back into the master example file.
## Please email <shawn.garbett@vanderbilt.edu> if this is required.
## 
## An event which stops the simulation must be included in the event
## list. Typically this would be a secular death event.
##
## MODIFY THIS FOR YOUR SIMULATION
##

library(simmer)

# Define simulation environment first
# IMPORTANT: this allows calls of now() inside trajectories
env  <- simmer("Simvastatin")

  ##############################################
 ##
## User defined counters / trackers
##
## MODIFY THIS LIST FOR YOUR SIMULATION
##
counters <- c("secular_death",
              "mild_myopathy",
              "mod_myopathy",
              "sev_myopathy",
              "CVD",
              "CVDDeath",
              "stopped",
              "switched",
              "drug1",
              "drug2",
              "drug3",
              "drug4",
              "life")

  ##############################################
 ##
## User defined events section
##
## An event which stops the simulation must be
## included in the event list. Typically this
## would be a secular death event.
##
## MODIFY THIS FOR YOUR SIMULATION
##

# Cleanup a death function, called for any form of death
# This is needed for use in any event that results in a
# death. One must closeout "in use" counters, otherwise they won't
# appear in statistics
cleanup_on_death <- function(traj)
{
  traj %>% 
  release("life") %>%
  branch(
    function(attrs) attrs[["CVDdrug"]]+1,
    merge=rep(TRUE,5),
    create_trajectory() %>% timeout(0),
    create_trajectory() %>% release("drug1"),
    create_trajectory() %>% release("drug2"),
    create_trajectory() %>% release("drug3"),
    create_trajectory() %>% release("drug4")
  )
}

source('event_secular_death.R')
source('event_myopathy.R')
source('event_cvd.R')

# Main event registry that is used by the event loop to create and track
# event in a generic manner. Each has a:
#   * name:          How you want it to appear in any printouts
#   * attr:          How it will be stored in the patient (trajectory) attributes
#   * time_to_event: a function given a list of attributes, returns in days
#                    how long till the next event. NOTE: The variable in
#                    must be named "attrs"
#   * func:          a function that given a trajectory, modifies it and
#                    returns the modified trajectory.
#
event_registry <- list(
  list(name          = "Secular Death",
       attr          = "eSecularTime",
       time_to_event = days_till_death,
       func          = secular_death),
  list(name          = "Mild Myopathy",
       attr          = "eMildMyoTime",
       time_to_event = days_till_mild_myopathy,
       func          = mild_myopathy),
  list(name          = "Moderate Myopathy",
       attr          = "eModMyoTime",
       time_to_event = days_till_mod_myopathy,
       func          = mod_myopathy),
  list(name          = "Severe Myopathy",
       attr          = "eSevMyoTime",
       time_to_event = days_till_sev_myopathy,
       func          = sev_myopathy),
  list(name          = "Cardiovascular Disease",
       attr          = "eCVDTime",
       time_to_event = days_till_cvd,
       func          = cvd)
)


  #################################################
 ##
## Assign patient attributes utilized in simulation
##
## MODIFY THIS FOR YOUR SIMULATION
##
##
assign_gender_and_age <- function(traj, inputs)
{
  # Assign Gender and Age based on Gender
  traj %>%
  seize("life") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.5, 0.5)),
    merge=c(TRUE,TRUE),
    create_trajectory("male") %>%
      set_attribute("gender", 1)                      %>%
      set_attribute("ageAtStart", inputs$vAge),
    create_trajectory("female") %>%
      set_attribute("gender", 2)                      %>%
      set_attribute("ageAtStart", inputs$vAge)
  ) %>%
  set_attribute("age", function(attrs) attrs[['ageAtStart']])
}

assign_cvd_genotype <- function(traj, inputs)
{
  # Assign Genotype
  traj %>% branch(
    function() sample(1:3, 1, prob=c(0.730, 0.249, 0.021)),
    merge=rep(TRUE,3),
    create_trajectory("TT") %>% set_attribute("CVDgenotype", 1),
    create_trajectory("TC") %>% set_attribute("CVDgenotype", 2),
    create_trajectory("CC") %>% set_attribute("CVDgenotype", 3)
  )
}

assign_cvd_medication <- function(traj, inputs)
{
  traj %>% branch(
    function(attrs) {
      # No treatment at all
      if(!inputs$vTX) return(5)
      
      # If there is no genotyping, or it's reactive, default to Simvastatin
      if(is.na(inputs$vPGx) || inputs$vPGx == "Reactive") return(1)
      
      # If the genotype is good metabolizer, then default to Simvastatin
      if(attrs[['CVDgenotype']] == 1) return(1)
      
      # Assign second line
      if(inputs$vSecondLine == "Atorvastin")          return(2)
      if(inputs$vSecondLine == "Rosuvastatin")        return(3)
      if(inputs$vSecondLine == "Low/Mod Dose Statin") return(4)
      
      # Something went very wrong
      stop("Invalid Logic in assigning cvd medication")
    },
    merge=rep(TRUE,5),
    create_trajectory("Simvastatin")  %>%
      set_attribute("CVDdrug", 1) %>%
      seize("drug1"),
    create_trajectory("Atorvastin")   %>%
      set_attribute("CVDdrug", 2) %>%
      seize("drug2") %>%
      mark("switched"),
    create_trajectory("Rosuvastatin") %>%
      set_attribute("CVDdrug", 3) %>%
      seize("drug3") %>%
      mark("switched"),
    create_trajectory("Low/Moderate Dose Statin") %>%
      set_attribute("CVDdrug", 4) %>%
      seize("drug4") %>%
      mark("switched"),
    create_trajectory("No Treatment") %>%
      set_attribute("CVDdrug", 0)
  )
}

## This function is referenced in main event loop
assign_attributes <- function(traj, inputs)
{
  # inputs$vTX  : boolean to treat or not to treat
  # inputs$vPGx : NA, "Preemptive", or "Reactive"
  # inputs$vSecondLine : "Atorvastin", "Rosuvastatin", or "Low/Mod Dose Statin"
  
  traj %>%
  assign_gender_and_age(inputs) %>%
    #timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0}) %>%
  assign_cvd_genotype(inputs)   %>%
    #timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0}) %>%
  assign_cvd_medication(inputs) #%>% timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0})
}

source('event_main_loop.R')

  ##############################################
 ##
## Piddly bits for development
##
##
##
# For testing right now
inputs      <- list()
inputs$vAge <- 40
inputs$vTX  <- FALSE
inputs$vPGx <- "Preemptive"
inputs$vSecondLine <- "Atorvastin"

library(dplyr)

traj <- simulation(env, inputs)

# Run the simulation 
env %>% create_counters(counters) %>%
        add_generator("patient", traj, at(rep(0, 1000)), mon=2) %>%
        run(36500) %>% # Simulate 100 years.
        wrap()

# Look at summary statistics
arrivals <- get_mon_arrivals(env, per_resource = T)

#hist(arrivals$start_time/365 + 40, main="Death by Natural Causes", xlab="Age")
hist(arrivals[arrivals$resource == 'secular_death',]$start_time/365+40, main="Natural Death", xlab="Age")

arrivals %>% count(resource)

#hist(arrivals[arrivals$resource == 'other_death',]$start_time/365+40, main="Other Death", xlab="Age")

