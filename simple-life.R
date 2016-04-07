
library(simmer)
library(dplyr)

# Translations of ARENA example
# (aGender == 2)+(aAge >= 75)+
# ((aAge > 65) && aAge < 75))+2*(aStrokeCount >0)
# Note 1: there's minor bug between powerpoint definition
#   and Arena example formula (i.e. it should be age >= 65)
#   this minor bug is kept to test reproducibility
# Note 2: This approach is more readible than Arena
scoreCHADS <- function(attribs)
{
  age         <- attribs[["age"]]
  gender      <- attribs[["gender"]]
  strokeCount <- attribs[["countStrokes"]]
  score       <- 0
  
  if (age >= 75)              score <- score + 2
  if (age < 75 && age > 65)   score <- score + 1
  if (gender == 2)            score <- score + 1
  if (strokeCount > 0)        score <- score + 2
  
  return(score)
}

strokeRisk <- function(attribs)
{
  CHAD <- attribs[["CHADS"]]
  
  annualRisk <- c(0.0, 1.3, 2.2, 3.2, 4.0, 6.7, 9.8, 9.6, 6.7, 15.2)/100
  
  return(annualRisk[CHAD+1])
}

# Check it out...
# x <- sapply(1:100000, function(x) rMaleAge())1800-
# hist(x, freq=FALSE, breaks=49:86)
rMaleAge <- function()
{
  probs <- c(rep(0.33/16, 16),
             rep(0.29/10, 10),
             rep(0.24/ 5,  5),
             rep(0.14/ 5,  5))
  
  sample(50:85, 1, prob=probs)
}

rFemaleAge <- function()
{
  probs <- c(rep(0.43/16, 16),
             rep(0.34/10, 10),
             rep(0.16/ 5,  5),
             rep(0.07/ 5,  5))
  
  sample(50:85, 1, prob=probs)
}

rPreviousStroke <- function()
{
  sample(0:2, 1, prob=c(0.65, 0.25, 0.1))
}

twoway <- function(attrs, attr)
{
  c(attrs[[attr]], 1-attrs[[attr]])
}

traj <- create_trajectory("ASPS") %>%
  # This is the configuration of a patient
  branch(function() sample(1:2, 1, prob=c(0.63, 0.37)),
         merge=c(T, T),
         create_trajectory("Male") %>%
           set_attribute("gender", 1) %>%
           set_attribute("age", rMaleAge),
         create_trajectory("Female") %>%
           set_attribute("gender", 2) %>%
           set_attribute("age", rFemaleAge)
  ) %>%
  set_attribute("previousStrokes", rPreviousStroke) %>%
  set_attribute("countStrokes",    function(attrs) attrs[["previousStrokes"]]) %>%
  # Re-entry here, i.e. loop on a year
  set_attribute("CHADS",           function(attrs) scoreCHADS(attrs)) %>%
  set_attribute("strokeRisk",      function(attrs) strokeRisk(attrs)) %>%
  branch(function(attrs) sample(1:2, 1, prob=twoway(attrs, "strokeRisk")),
         merge=c(T, T),
         create_trajectory("Stroke Event") %>%
           #timeout(function() {print("stroke event!"); return(0);}) %>%
           seize("strokes", 1) %>%
           set_attribute("countStrokes",    function(attrs) 1+attrs[["countStrokes"]]) %>%
           release("strokes") %>%
           branch(function() sample(1:2, 1, prob=c(0.81, 0.19)),
                  merge=c(T, F),
                  create_trajectory("Survived Stroke") %>% timeout(0),
                  create_trajectory("Fatality") %>% seize("deaths", 1) %>% release("deaths")
           ) %>% timeout(0),
         create_trajectory("No Stroke") %>% timeout(0)
        ) %>%
  set_attribute("age", function(attrs) attrs[["age"]]+1) %>%
  timeout(1) %>%
  rollback(amount=5,times=5) # Do it 5 more times, for 6 years total

env <- simmer("Sim ARENA Toy") %>%
  add_generator("patient", traj, at(rep(0, 50)), mon=2) %>%
  add_resource("strokes", Inf, 0) %>%
  add_resource("deaths",  Inf, 0) %>%
  run() %>%
  wrap()

arrivals <- get_mon_arrivals(env, per_resource = T)

# Times that each branch was entered
arrivals %>% count(resource)

