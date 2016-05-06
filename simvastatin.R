library(shiny)
library(simmer)
library(dplyr)

source('age-weibull.R')

counters <- c("natural_death",
              "mild_myopathy",
              "mod_myopathy",
              "sev_myopathy",
              "CVD",
              "stopped",
              "switched",
              "drug4")

# Helper functions for managing counters
create_counters <- function(env, counters)
{
    sapply(counters, FUN=function(counter)
    {
        env <- add_resource(env, counter, Inf, 0)
    })

    env
}

mark <- function(traj, counter)
{
    traj               %>%
    seize(counter,1)   %>%
    timeout(0)         %>%
    release(counter,1)
}

# Assign gender and secular death relative to gender
assign_gender <- function(traj, age)
{
    traj %>% branch(function()
        sample(1:2, 1, prob=c(0.5, 0.5)),
        merge=c(TRUE,TRUE),
        create_trajectory("male") %>%
            set_attribute("gender", 1)                      %>%
            set_attribute("age", age)                       %>%
            set_attribute("ageAtDeath", function(attrs) ageAtDeath(age, 1)) %>%
            set_attribute("daysTillDeath",
                function(attrs) {365*(attrs[["ageAtDeath"]]-age)}),
        create_trajectory("female") %>%
            set_attribute("gender", 2)                      %>%
            set_attribute("age", age)                       %>%
            set_attribute("ageAtDeath", function(attrs) ageAtDeath(age, 2)) %>%
            set_attribute("daysTillDeath",
                function(attrs) {365*(attrs[["ageAtDeath"]]-age)})
    )
}

# Assign population genotype
assign_genotype <- function(traj)
{
    traj %>% branch(
        function() sample(1:3, 1, prob=c(0.730, 0.249, 0.021)),
        merge=rep(TRUE,3),
        create_trajectory("TT") %>% set_attribute("genotype", 1),
        create_trajectory("TC") %>% set_attribute("genotype", 2),
        create_trajectory("CC") %>% set_attribute("genotype", 3)
    )
}

assign_medication <- function(traj)
{
    traj
}

next_event <- function(traj, env)
{
    # The first thing in this must be a branch, to keep the rollback consistent
    traj %>% branch( 
        function(attrs) 1,
        merge=c(FALSE),
        create_trajectory("secular_death") %>% 
          timeout(function(attrs) attrs[["daysTillDeath"]]) %>% 
          mark("natural_death")
    )
}

simvastatin <- function(env, inputs)
{
    create_trajectory("Simvastatin") %>%
    assign_gender(inputs$vAge)       %>%
    assign_genotype()                %>%
    assign_medication()              %>%
    next_event(env)                  %>% 
    rollback(amount=1, times=100) # Process up to 100 events per person
}

# For testing right now
inputs      <- list()
inputs$vAge <- 40

env  <- simmer("RIGHT")
traj <- simvastatin(env, inputs)

env %>%  create_counters(counters) %>%
         add_generator("patient", traj, at(rep(0, 15000)), mon=2) %>%
         run(36500) %>% # Simulate 100 years.
         wrap()

arrivals <- get_mon_arrivals(env, per_resource = T)

hist(arrivals$start_time/365 + 40, main="Death by Natural Causes", xlab="Age")
