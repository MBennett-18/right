
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
  if (gender == "F")          score <- score + 1
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

rAge <- function(gender)
{
  if(gender == "M")
  {
    rMaleAge()
  }
  else
  {
    rFemaleAge()
  }
}

rPreviousStroke <- function()
{
  sample(0:2, 1, prob=c(0.65, 0.25, 0.1))
}


asps <- function()
{
  gender      <- sample(c("M","F"), 1, prob=c(0.63, 0.37))
  age         <- rAge(gender)
  prevStrokes <- rPreviousStroke()
  nStrokes    <- prevStrokes
  nEvents     <- 0
  nFatalities <- 0
  
  for(years in 1:6)
  {
    risk      <- strokeRisk(scoreCHADS(gender, age, nStrokes))
    if(runif(1) <= risk)
    {
      nStrokes <- nStrokes + 1
      nEvents  <- nEvents + 1
      if(runif(1) <= 0.19)
      {
        nFatalities <- 1
        break
      }
    }
    age <- age + 1
  }
  
  c(nEvents, nFatalities)
}

sim <- function() rowSums(sapply(1:50, FUN=function(x) asps()))

ptm <- proc.time()
rowSums(sapply(1:10000, function(x) sim()))/10000
proc.time() - ptm