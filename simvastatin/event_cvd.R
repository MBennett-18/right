library(simmer)

age_bracket <- function(attrs)
{
  age <- attrs[['age']]
  
  if (age <= 40) { return(1) } else
  if (age <= 45) { return(2) } else
  if (age <= 50) { return(3) } else
  if (age <= 55) { return(4) } else
  if (age <= 60) { return(5) } else
  if (age <= 65) { return(6) } else
  if (age <= 70) { return(7) } else
  if (age <= 75) { return(8) }
  
  return(9)
}

days_till_cvd <- function(attrs)
{
  drug       <- attrs[['CVDdrug']]
  gender     <- attrs[['gender']]
  bracket    <- age_bracket(attrs)
  time_frame <- 3650
  probCVDf <- c(0.02071300,
                0.03951050,
                0.05464350,
                0.06501275,
                0.12643400,
                0.15508900,
                0.18232800,
                0.20793550,
                0.23554300)
  probCVDm <- c(0.04010200,
                0.06818350,
                0.09897900,
                0.01326720,
                0.19648750,
                0.23077900,
                0.27363950,
                0.30658550,
                0.34763400)
  
  rr <- if(drug==4) {0.75} else
        if(drug==0) {1}    else {0.65}
  
  risk <- if(gender == 1) {probCVDm[bracket]} else {probCVDf[bracket]}
  
  rate <- -log(1-risk)*rr/time_frame
  
  rexp(1, rate)
}

cvd <- function(traj)
{
  traj %>%
  mark("cvd") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.117, 0.883)),
    merge=c(FALSE, TRUE),
    create_trajectory("CVD w/ Death") %>% mark("cvd_death") %>% cleanup_on_death(),
    create_trajectory("CVD Event") %>% timeout(0)
  )
}