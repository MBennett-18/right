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
library(shiny)
library(simmer)
library(dplyr)


# Define simulation environment first
# Important: this allows calls of now() inside trajectories
env  <- simmer("Simvastatin")

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

## Secular Death
source('age-weibull.R')


# Cleanup a death, 
# i.e. closeout "in use" counters
cleanup_on_death <- function(traj)
{
  traj %>% branch(
    function(attrs) attrs[["CVDdrug"]]+1,
    merge=rep(TRUE,5),
    create_trajectory() %>% timeout(0),
    create_trajectory() %>% release("drug1"),
    create_trajectory() %>% release("drug2"),
    create_trajectory() %>% release("drug3"),
    create_trajectory() %>% release("drug4")
  )
}


# Given attributes of a patient (trajectory), it returns in days 
# how long till the patient would die a secular death.
#
# NOTE: The variable in must be named attrs
days_till_death <- function(attrs)
{
  age       <- attrs[['age']]
  death_age <- ageAtDeath(age, attrs[['gender']])
  
  return(365*(death_age-age))
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
secular_death <- function(traj)
{
  traj %>% branch(
    function() 1,
    merge=c(FALSE), # False is patient death
    create_trajectory("Secular Death") %>% mark("natural_death") %>% cleanup_on_death()
  )
}

# Mild Myopathy events
days_till_mild_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]

  time_frame <- 1825 # 5 Years
  risk       <- if(drug == 0) 1e-7 else 0.05
  rate       <- -log(1-risk)/time_frame
  
  return(rexp(1, rate))
}

# Mark a mild myopathy event
mild_myopathy <- function(traj)
{
  traj %>%
  mark("mild_myopathy")
}

# Moderate myopathy events
days_till_mod_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
  gt   <- attrs[["CVDgenotype"]]
  
  rr <- if(drug == 1)
  {
    c(1, 2.55, 9.56)[gt]
  } else if(drug == 2)
  {
    c(1, 1.08, 4.05)[gt]
  } else
  {
    1
  }
  
  time_frame <- 365 # 1 Years
  risk       <- if(drug == 0) 1e-10 else 0.00011
  rate       <- -log(1-risk)*rr/time_frame
  
  return(rexp(1, rate))
}

# Mark a moderate myopathy event
mod_myopathy <- function(traj)
{
  traj %>%
    mark("mod_myopathy")
}

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
       func          = mod_myopathy)
)

  ##############################################
 ##
## User defined counters
##
## MODIFY THIS LIST FOR YOUR SIMULATION
##
counters <- c("natural_death",
              "mild_myopathy",
              "mod_myopathy",
              "sev_myopathy",
              "CVD",
              "stopped",
              "switched",
              "drug1",
              "drug2",
              "drug3",
              "drug4")

  #################################################
 ##
## Assign patient attributes utilized in simulation
##
## MODIFY THIS FOR YOUR SIMULATION
##
assign_gender_and_age <- function(traj, inputs)
{
  # Assign Gender and Age based on Gender
  traj %>% branch(
    function() sample(1:2, 1, prob=c(0.5, 0.5)),
    merge=c(TRUE,TRUE),
    create_trajectory("male") %>%
      set_attribute("gender", 1)                      %>%
      set_attribute("age", inputs$vAge),
    create_trajectory("female") %>%
      set_attribute("gender", 2)                      %>%
      set_attribute("age", inputs$vAge)
  )
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

  ##############################################
 ##
## Helper functions for managing counters
##
## Hopefully, no modification required.
##

# Create the counters, takes a list
create_counters <- function(env, counters)
{
    sapply(counters, FUN=function(counter)
    {
        env <- add_resource(env, counter, Inf, 0)
    })

    env
}

# Mark a counter
mark <- function(traj, counter)
{
    traj               %>%
    seize(counter,1)   %>%
    timeout(0)         %>%
    release(counter,1)
}

  ##############################################
 ##
## Helper functions for managing events
##
## Hopefully, no modification required.
##
assign_events <- function(traj, inputs)
{
  sapply(event_registry, FUN=function(event)
  {
    traj <- set_attribute(traj, event$attr, function(attrs)
    {
      event$time_to_event(attrs)
    })
  })
  traj
}

# Find the next event based on time
next_event <- function(attrs)
{
  event_time <- Inf
  event      <- NA
  id         <- 0
  for(i in 1:length(event_registry))
  {
    e <- event_registry[[i]]
    tmp_time   <- attrs[[e$attr]]
    if(tmp_time < event_time)
    {
      event      <- e
      event_time <- tmp_time
      id         <- i
    }
  }
  
  return(list(event=event, event_time=event_time, id=id))
}

# Process events in main loop
process_events <- function(traj, env)
{
  # Find the next event from possible events, and timeout (wait) till that moment
  traj <- timeout(traj, function(attrs)
    {
      # Determine next up
      ne <- next_event(attrs)
      event <- ne[['event']]
      event_time <- ne[['event_time']]
    
      #cat(" Next up => ",event$name,"\n")
      #cat("            waiting", event_time-now(env), '\n')
    
      # Wait the clock time for the nearest event, minus now()
      event_time - now(env)
    })
  
  # Create a handler for every possible event, using their
  # list position as the branch number
  # This will determine the id of the next event
  # Call it's modification function
  # and then update it's next time to event
  args <- lapply(event_registry,FUN=function(e) {
    create_trajectory(e$name) %>%
      e$func() %>%
      set_attribute(e$attr, function(attrs) {now(env)+e$time_to_event(attrs)})
  })
  args$traj   <- traj
  args$option <- function(attrs) next_event(attrs)$id
  args$merge  <- rep(TRUE,length(event_registry))

  do.call(branch, args)
}

  ##############################################
 ##
## MAIN LOOP
##
## This should not require modification
## This creates a patient simulation (trajectory)
## 
## It uses a branch in a manner to prevent the
## rollback from looking further up the stack
## of the event loop. 
##
simulation <- function(env, inputs)
{
    create_trajectory("Patient")     %>%
    assign_attributes(inputs)        %>%
    assign_events(inputs)            %>%
    branch( # Used branch, to prevent rollback from looking inside event loop function
      function() 1,
      merge=TRUE,
      create_trajectory("main_loop") %>% process_events(env)
    ) %>% 
    rollback(amount=1, times=100) # Process up to 100 events per person
}

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
inputs$vPGx <- "Preemptive"
inputs$vSecondLine <- "Atorvastin"

traj <- simulation(env, inputs)

# Run the simulation 
env %>%  create_counters(counters) %>%
         add_generator("patient", traj, at(rep(0, 1000)), mon=2) %>%
         run(36500) %>% # Simulate 100 years.
         wrap()

# Look at summary statistics
arrivals <- get_mon_arrivals(env, per_resource = T)

#hist(arrivals$start_time/365 + 40, main="Death by Natural Causes", xlab="Age")
hist(arrivals[arrivals$resource == 'natural_death',]$start_time/365+40, main="Natural Death", xlab="Age")

arrivals %>% count(resource)

#hist(arrivals[arrivals$resource == 'other_death',]$start_time/365+40, main="Other Death", xlab="Age")

