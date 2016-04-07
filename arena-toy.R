library(simmer)


# Simmer extension to allow for many trajectories at the same time
n_at <- function(n, t)
{
  function()
  {
    if (n > 0)
    {
      n <- n - 1
      return(t)
    }
    else
    {
      return(-1)
    }
  }
}

# Translations of ARENA example
# (aGender == 2)+(aAge >= 75)+
# ((aAge > 65) && aAge < 75))+2*(aStrokeCount >0)
# Note 1: there's minor bug between powerpoint definition
#   and Arena example formula (i.e. it should be age >= 65)
#   this minor bug is kept to test reproducibility
# Note 2: This approach is more readible than Arena
scoreCHADS <- function(gender, age, strokeCount)
{
  score <- 0
  if (age >= 75)              score <- score + 2
  if (age < 75 && age > 65)   score <- score + 1
  if (gender == 2)            score <- score + 1
  if (strokeCount > 0)        score <- score + 2
  return(score)
}

strokeRisk <- function(CHAD)
{
  annualRisk <- c(0, 1.3, 2.2, 3.2, 4.0, 6.7, 9.8, 9.6, 6.7, 15.2)/100
  return(annualRisk[CHAD+1])
}


# Check it out...
# x <- sapply(1:100000, function(x) rMaleAge())
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


traj <- create_trajectory("ASPS") %>%
  # This is the configuration of a patient
  branch(function() sample(1:2, 1, prob=c(0.63, 0.37)),
         merge=c(T, T),
    create_trajectory("Male") %>%
      set_attribute("gender", 1) %>%
      set_attribute("age", rMaleAge())
    create_trajectory("Female") %>%
      set_attribute("gender", 2),
      set_attribute("age", rFemaleAge())
  ) %>%
  set_attribute("previousStrokes", rPreviousStroke()) %>%
  set_attribute("countStrokes",    attrs[["previousStrokes"]]) %>%
  # Re-entry here, i.e. loop on a year
  # XXX ???
  set_attribute("CHADS",           scoreCHADS(attrs[["gender"]], attrs[["age"]], attr[["countStrokes"]])) %>%
  set_attribute("strokeRisk",      strokeRisk(attrs[["CHADS"]])) %>%
  branch(function() sample(1:2, 1, prob=c(attrs[["strokeRisk"]], 1-attrs[["strokeRisk"]])),
                    merge=c(T, T),
    create_trajectory("Stroke Event") %>%
      set_attribute("countStrokes", attrs[["countStrokes"]]+1) %>%
      branch(function) sample(1:2, 1, prob=c(0.81, 0.19)),
        merge=c(T, F),
        create_trajectory("Survived Stroke"),
        create_trajectory("Fatality")
      ),
    create_trajectory("No Stroke"),
  ) %>%
  timeout(1) %>%
  set_attribute("age", attrs[["age"]]+1) %>%
  XXX ??? How to loop back to rentry here.


env <- simmer("Sim ARENA Toy") %>%
       add_generator("patient", traj, n_at(10, 0)) %>%
       run(until=5) %>%
       wrap()