  ###########################################################################
 ##
##  Assign and Track Cardiovascular Events
##
## Risk is a composite based on age and gener
## Source: http://chd.bestsciencemedicine.com/calc2.html
## Risks of heart attacks, angina/coronary insufficiency, heart failure, 
## stroke and intermittent claudication based on data from Framingham
## Cohort Study.
##
## Method derived from
## D'Agostino, Vasan, Pencina, Wolf, Cobain, Massaro, Kannel,
## "General Cardiovascular Risk Profile for Use in Primary Care: The
##  Framingham Heart Study". Circulation, 2008:117:743-753. 

library(simmer)

source('cvd_framingham.R')

days_till_reassess_cvd <- function(attrs) { 3650.0 }

reassess_cvd <- function(traj)
{
  traj # Does nothing, but triggers reassessment. 
}

days_till_cvd <- function(attrs)
{
  gender     <- attrs[['gender']]
  age        <- attrs[['age']]

  tot_chol   <- attrs[['totChol']]
  hdl_chol   <- attrs[['hdlChol']]

  prob <- if(gender == 1)
            cvd_prob_10_year_male_framingham(  age, tot_chol, hdl_chol)
          else
            cvd_prob_10_year_female_framingham(age, tot_chol, hdl_chol)

  rexp(1, prob / 3650)
}

cvd <- function(traj)
{
  traj %>%
  mark("cvd") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.117, 0.883)),
    merge=c(FALSE, TRUE),
    create_trajectory("CVD w/ Death") %>% mark("cvd_death") %>% cleanup(),
    create_trajectory("CVD Event") %>% timeout(0)
  )
}