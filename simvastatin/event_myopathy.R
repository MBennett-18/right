library(simmer)


# Mild Myopathy events
days_till_mild_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
  
  time_frame <- 1825 # 5 Years
  risk       <- if(drug == 0) 1e-7 else 0.05
  rate       <- -log(1-risk)/time_frame
  
  return(rexp(1, rate))
}

# Mark a mild myopathy event
mild_myopathy <- function(traj)
{
  traj %>%
    mark("mild_myopathy")
}

# Moderate myopathy events
days_till_mod_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
  gt   <- attrs[["CVDgenotype"]]
  
  rr <- if(drug == 1)
  {
    c(1, 2.55, 9.56)[gt]
  } else if(drug == 2)
  {
    c(1, 1.08, 4.05)[gt]
  } else
  {
    1
  }
  
  time_frame <- 365 # 1 Year
  risk       <- if(drug == 0) 1e-10 else 0.00011
  rate       <- -log(1-risk)*rr/time_frame
  
  return(rexp(1, rate))
}

# Mark a moderate myopathy event
mod_myopathy <- function(traj)
{
  traj %>%
    mark("mod_myopathy")
}

# Severe myopathy events
days_till_sev_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
  gt   <- attrs[["CVDgenotype"]]
  
  rr <- if(drug == 1)
  {
    c(1, 2.55, 9.56)[gt]
  } else if(drug == 2)
  {
    c(1, 1.08, 4.05)[gt]
  } else
  {
    1
  }
  
  time_frame <- 365 # 1 Year
  risk       <- if(drug == 0) 1e-16 else 0.000034
  rate       <- -log(1-risk)*rr/time_frame
  
  return(rexp(1, rate))
}

# Mark a severe myopathy event
sev_myopathy <- function(traj)
{
  traj %>%
    mark("sev_myopathy")
}