library(RUnit)

source("simvastatin/cvd_framingham.R")

test.male <- function()
{
  # From Appendix Examples in paper (center is off)
  checkEqualsNumeric(cvd_prob_10_year_male_framingham(53, 161, 55, 125, TRUE, FALSE, TRUE),
                     0.1450673)
  
  # Test Center
  checkEqualsNumeric(1-cvd_prob_10_year_male_framingham(48.5),
                     0.88936, tolerance=1e-5)
  
}

test.female <- function()
{
  # From Appendix Examples in paper (center is off)
  checkEqualsNumeric(cvd_prob_10_year_female_framingham(61, 180, 47, 124, FALSE, TRUE, FALSE),
                     0.096181007)
  
  # Test Center
  checkEqualsNumeric(1-cvd_prob_10_year_female_framingham(49.1),
                     0.95012, tolerance=1e-5)
}

test.vectorize.male <- function()
{
  x <- cvd_prob_10_year_male_framingham(c(40, 80))
  checkEquals(length(x), 2)
}

test.vectorize.female <- function()
{
  x <- cvd_prob_10_year_female_framingham(c(40, 80))
  checkEquals(length(x), 2)
}