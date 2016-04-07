library(simmer)

t0 <- create_trajectory() %>%
  branch(function() sample(c(1, 2), 1), c(T, T),
        create_trajectory() %>%
          seize("branch1", 1) %>%
          # do stuff here
          timeout(function() rexp(1, 1)) %>%
          release("branch1", 1),
        create_trajectory() %>%
          seize("branch2", 1) %>%
          # do stuff here
          timeout(function() rexp(1, 2)) %>%
          release("branch2", 1))

env <- simmer() %>%
  add_generator("dummy", t0, at(rep(0, 1000))) %>%
  # Resources with infinite capacity, just for accounting purposes
  add_resource("branch1", Inf, 0) %>%
  add_resource("branch2", Inf, 0)

env %>% run()
arrivals <- get_mon_arrivals(env, per_resource = T)

library(dplyr)
# Times that each branch was entered
arrivals %>% count(resource)

library(ggplot2)
# The `activity_time` is the total time inside each branch for each arrival
# Let's see the distributions
ggplot(arrivals) + geom_histogram(aes(x=activity_time)) + facet_wrap(~resource)
