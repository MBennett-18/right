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
env  <- simmer("CVD")

  ##############################################
 ##
## User defined counters / trackers
##
## MODIFY THIS LIST FOR YOUR SIMULATION
##
counters <- c("secular_death",
              "cvd",
              "cvd_death",
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
cleanup <- function(traj)
{
  traj %>% 
  release("life")
}

source('event_secular_death.R')
source('event_cvd.R')

  #################################################################################
 #
#' Main event registry that is used by the event loop to create and track
#' events in a generic manner.
#'
#' @param name          How it will appear in printouts
#' @param attr          The trajectory attribute storing time to next event
#' @param time_to_event A function that given a list of attributes, returns in days
#'                      how long till the next event. NOTE: The variable in
#'                      must be named "attrs"
#' @param func          A function that given a trajectory, modifies it for an
#'                      event's occurrance.
#' @param reactive      Redraw the time_to_event if another event has triggered.
#' @export
event_registry <- list(
  list(name          = "Secular Death",
       attr          = "eSecularTime",
       time_to_event = days_till_death,
       func          = secular_death,
       reactive      = FALSE),
  list(name          = "Cardiovascular Disease",
       attr          = "eCVDTime",
       time_to_event = days_till_cvd,
       func          = cvd,
       reactive      = TRUE),
  list(name          = "Reassess CVD Risk",
       attr          = "eCVDReassess",
       time_to_event = days_till_reassess_cvd,
       func          = reassess_cvd,
       reactive      = FALSE)
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
      set_attribute("ageAtStart", function() 20.0),
    create_trajectory("female") %>%
      set_attribute("gender", 2)                      %>%
      set_attribute("ageAtStart", function() 20.0)
  ) %>%
  set_attribute("age", function(attrs) attrs[['ageAtStart']]) %>%

  set_attribute("totChol", function() rnorm(1, 228, 39)) %>% # From MRC/BHF Heart Study, Lancet 2002
  set_attribute("hdlChol", function() rnorm(1, 40.9, 0.3861)) # Only recording entering value
}

## This function is referenced in main event loop
assign_attributes <- function(traj, inputs)
{
  traj %>%
  assign_gender_and_age(inputs)
}

source('event_main_loop.R')

cvd <- function(inputs, N=14000)
{
    env  <<- simmer("CVD") %>% create_counters(counters)

    traj  <- simulation(env, inputs)

    env %>% add_generator("patient", traj, at(rep(0, N)), mon=2)
    env %>% run(36500)  # Simulate 100 years.

    arrivals <- get_mon_arrivals(env, per_resource = T)

    arrivals$resource <- factor(arrivals$resource, counters)

    # 30 day events
    events_to_fix_end <- c("cvd")
    arrivals[arrivals$resource %in% events_to_fix_end,]$end_time <- arrivals[arrivals$resource %in% events_to_fix_end,]$start_time + 1.0
  
    # Compute total activity times
    arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
    arrivals
}