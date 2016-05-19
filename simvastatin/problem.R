library(simmer)

env <- simmer("Global Env for now() to work") # Not used in this example, but main code needs this

# Configuration for model run, will be called from another R package with this info
# Doing this as a global
input <- TRUE

# Create some resources
env %>% add_resource("life", Inf, 0)

# May terminate
event <- function(traj)
{
  traj %>% branch(
    function() sample(1:2, 1, prob=c(0.8,0.2)),
    merge=c(TRUE,FALSE),
    create_trajectory() %>% timeout(1), # Something non-fatal happened
    create_trajectory() %>% release("life")
  )
}

traj <- create_trajectory() %>%
        seize("life") %>%
        branch(function() input+1,
               merge=c(FALSE,TRUE),
               create_trajectory() %>%
                 timeout(40) %>%
                 release("life"),
               create_trajectory() %>%
                 event()
        ) %>%
        rollback(amount=1, times=100) 

env %>% add_generator("patient", traj, at(rep(0, 10)), mon=2)

run_sim <- function(conf)
{
  input <- conf # Change global conf
  env %>% reset() %>% run(36500) %>% wrap()
  get_mon_arrivals(env, per_resource = T)
}

run_sim(TRUE)

run_sim(FALSE) # NO OUTPUT!!!!, doesn't matter if configuration is flipped, the 2nd run is empty

# Well that didn't work, now for hacked permutative guessing
run_sim <- function(conf)
{
  input <- conf # Change global conf

  env %>% reset()
  env %>% add_generator("patient", traj, at(rep(0, 10)), mon=2)
  env %>% run(36500) %>% wrap()
  get_mon_arrivals(env, per_resource = T)
}

run_sim(TRUE) # Still no output, probably didn't need generator

# Hack #2, Seems to work--but does weird things in a larger simulation around resources.
run_sim <- function(conf)
{
  input <- conf # Change global conf

  # Blow global environment away!!!  
  env <- simmer("Global Env for now() to work") 
  env %>% add_generator("patient", traj, at(rep(0, 10)), mon=2)
  env %>% add_resource("life", Inf, 0)
  env %>% run(36500) %>% wrap()
  get_mon_arrivals(env, per_resource = T)
}

run_sim(TRUE)

run_sim(FALSE)