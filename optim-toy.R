
# Translations of ARENA example
# (aGender == 2)+(aAge >= 75)+
# ((aAge > 65) && aAge < 75))+2*(aStrokeCount >0)
# Note 1: there's minor bug between powerpoint definition
#   and Arena example formula (i.e. it should be age >= 65)
#   this minor bug is kept to test reproducibility
# Note 2: This approach is more readible than Arena
scoreCHADS <- cmpfun(function(gender, age, strokeCount)
{
  score <- 0
  if (age >= 75)              score <- score + 2
  if (age < 75 && age > 65)   score <- score + 1
  if (gender == "F")          score <- score + 1
  if (strokeCount > 0)        score <- score + 2
  return(score)
})

annualRisk <- c(0, 1.3, 2.2, 3.2, 4.0, 6.7, 9.8, 9.6, 6.7, 15.2)/100

strokeRisk <- cmpfun(function(CHAD)
{
  return(annualRisk[CHAD+1])
})


# Check it out...
# x <- sapply(1:100000, function(x) rMaleAge())
# hist(x, freq=FALSE, breaks=49:86)
mProbs <- c(rep(0.33/16, 16),
           rep(0.29/10, 10),
           rep(0.24/ 5,  5),
           rep(0.14/ 5,  5))

fProbs <- c(rep(0.43/16, 16),
           rep(0.34/10, 10),
           rep(0.16/ 5,  5),
           rep(0.07/ 5,  5))

rAge <- cmpfun(function(gender)
{
  if(gender == "M")
  {
    sample(50:85, 1, prob=mProbs)
  }
  else
  {
    sample(50:85, 1, prob=fProbs)
  }
})

previousProb=c(0.65, 0.25, 0.1)
rPreviousStroke <- cmpfun(function()
{
  sample(0:2, 1, prob=previousProb)
})

# Moved accumulators outside
sim2 <- function(n)
{
  nEvents     <- 0
  nFatalities <- 0
  
  gender      <- "M"
  age         <- 0
  prevStrokes <- 0
  nStrokes    <- 0

  while(n > 0)
  {
    gender      <- sample(c("M","F"), 1, prob=c(0.63, 0.37))
    age         <- rAge(gender)
    nStrokes    <- rPreviousStroke()
    year        <- 0

    while(year <= 5)
    {
      risk      <- strokeRisk(scoreCHADS(gender, age, nStrokes))
      if(runif(1) <= risk)
      {
        nStrokes <- nStrokes + 1
        nEvents  <- nEvents  + 1
        if(runif(1) <= 0.19)
        {
          nFatalities <- nFatalities + 1
          break
        }
      }
      age  <- age + 1
      year <- year + 1
    }
  
    n <- n - 1
  }
  c(nEvents, nFatalities)
}

sim3 <- cmpfun(sim2)

ptm <- proc.time()
sim3(50*1000) / 1000
proc.time() - ptm