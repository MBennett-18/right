# Example Model Run Inputs as R list (with sublists for specific drugs)

inputs <- list(
  # Population Parameters
  vN           = 10000,   # Patients to simulate
  vLowerAge    = 40,      # Lower age to simulate coming in (uniform distribution)
  vUpperAge    = 40,      # Upper age to simulate
  vHorizon     = 10,      # Length of simulation upon a patient entering
  
  # Strategies
  vPremptive   = "None",  # Can be one of following: "None", "Panel", "PREDICT", or "Age >= 50"
  vReactive    = "Panel", # Can be one of following: "None", "Single", "Panel"
  vPanel       = list(vSimvastatin = TRUE, vWarfarin=FALSE, vClopidgrel = FALSE),

  # Single Gene
  vCostPanelGene  = 250, # Cost to genotype a full panel
  vCostSingleGene = 100, # Cost to genotype single condition

  # Drug specific
  clopidogrel = list(
    vPREDICTsens = 0.3,    
    vPREDICTspec = 0.3,
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
    yadda_yadda = TRUE # ... More here
  ),
  warfarin = list(
    vPREDICTsens = 0.3,    
    vPREDICTspec = 0.3,
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
    yadda_yadda = TRUE # ... More here
  ),
  simvastatin = list(
    vPREDICTsens = 0.3,    
    vPREDICTspec = 0.3,
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
    
    vProbSimvastatinAlt = 1,  # Prob. of Alt | Variant
    vProbSimStopMild = 0.23,  # Prob. of Stop | Mild Myo
    vProbSimStopMod  = 0.23,  # Prob. of Stop | Mod Myo
    vProbSimStopSev  = 1.00,  # Prob. of Stop | Sev Myo
 
    # Mild Myopathy Risks
    vMildMyoBaseNoVar=1e-7, # No Drug Risk of mild myopathy
    vMildMyoSimNoVar=0.05,  # Simvastatin Mild Myopathy Baseline Risk
    vMildMyoSimMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoSimPoorVar=1,   # Rel Risk|Poor metabolizer
    vMildMyoAltNoVar=0.05,  # Alternate Drug Mild Myopathy Baseline Risk
    vMildMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoAltPoorVar=1,    # Rel Risk|Poor metabolizer

    # Moderate Myopathy Risks
    vModMyoBaseNoVar=1e-10, # No Drug Risk of mild myopathy
    vModMyoSimNoVar=0.00011,  # Simvastatin Mild Myopathy Baseline Risk
    vModMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vModMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vModMyoAltNoVar=0.00011,  # Alternate Drug Mild Myopathy Baseline Risk
    vModMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vModMyoAltPoorVar=4.05,    # Rel Risk|Poor metabolizer

    # Moderate Myopathy Risks
    vSevMyoBaseNoVar=1e-16,   # No Drug Risk of mild myopathy
    vSevMyoSimNoVar=0.000034, # Simvastatin Mild Myopathy Baseline Risk
    vSevMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vSevMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vSevMyoAltNoVar=0.000034, # Alternate Drug Mild Myopathy Baseline Risk
    vSevMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vSevMyoAltPoorVar=4.05,   # Rel Risk|Poor metabolizer

    vCostSimvastatin=147,     # Yearly cost of simvastatin
    vCostAlternate=173.1      # Yearly cost of alternative
  ),
  # If these names match the event names from the simmer model, then computation can be generalized!
  costs = list(
    mild_myopathy = 129,
    mod_myopathy = 2255,
    sev_myopathy = 12811,
    cvd          = 20347
  ),
  durations = list(
    mild_myopathy = 1,
    mod_myopathy  = 30,
    sev_myopathy  = 30,
    cvd           = 30
  ),
  disutilities = list(
    mild_myopathy = 0.01,
    mod_myopathy  = 0.05,
    sev_myopathy  = 0.53,
    cvd           = 0.2445
  )
  
)   

