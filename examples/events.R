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
source('examples/age-weibull.R')

# Given attributes of a patient (trajectory), it returns in days 
# how long till the patient would die a secular death.
#
# NOTE: The variable in must be named attrs
daysTillDeath <- function(attrs)
{
  age      <- attrs[['age']]
  deathAge <- ageAtDeath(age, attrs[['gender']])
  
  return(365*(deathAge-age))
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
secularDeath <- function(traj)
{
  traj %>% branch(
    function() 1,
    merge=c(FALSE), # False is patient death
    create_trajectory("Secular Death") %>% mark("natural_death")
  )
}

# Random event 'other' example
daysTillOther <- function(attrs)
{
  t2e <- rexp(1, 0.001)

  return(t2e)
}

# This other will only have a chance of dying. So the branch
# has two outcomes.
otherEvent <- function(traj)
{
  traj %>%
  mark("other_event") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.05,0.95)),
    merge=c(FALSE,TRUE), # False is patient death
    create_trajectory("Other Death") %>%
      mark("other_death"),
    create_trajectory("Event Passes w/o Incident") %>%
      timeout(0)
  )
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
       time_to_event = daysTillDeath,
       func          = secularDeath),
  list(name          = "Other Event",
       attr          = "eOtherTime",
       time_to_event = daysTillOther,
       func          = otherEvent)
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
              "drug4",
              "other_death",
              "other_event")



  #################################################
 ##
## Assign patient attributes utilized in simulation
##
## MODIFY THIS FOR YOUR SIMULATION
##
assign_attributes <- function(traj, inputs)
{
  traj %>% 
  # Assign Gender and Age based on Gender
  branch(function()
    sample(1:2, 1, prob=c(0.5, 0.5)),
    merge=c(TRUE,TRUE),
    create_trajectory("male") %>%
      set_attribute("gender", 1)                      %>%
      set_attribute("age", inputs$vAge),
    create_trajectory("female") %>%
      set_attribute("gender", 2)                      %>%
      set_attribute("age", inputs$vAge)
  ) %>%
  # Assign Genotype
  branch(
    function() sample(1:3, 1, prob=c(0.730, 0.249, 0.021)),
    merge=rep(TRUE,3),
    create_trajectory("TT") %>% set_attribute("genotype", 1),
    create_trajectory("TC") %>% set_attribute("genotype", 2),
    create_trajectory("CC") %>% set_attribute("genotype", 3)
  ) # %>% timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0})
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
    
#      cat(" Next up => ",event$name,"\n")
#      cat("            waiting", event_time-now(env), '\n')
    
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
hist(arrivals[arrivals$resource == 'other_death',]$start_time/365+40, main="Other Death", xlab="Age")

