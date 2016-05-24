


age     <- c(40, 45, 50, 55, 60, 65, 70, 75, 80)
arena.m <- c(0.04010200,
             0.06818350,
             0.09897900,
             0.01326720,
             0.19648750,
             0.23077900,
             0.27363950,
             0.30658550,
             0.34763400)
arena.f <- c(0.02071300,
             0.03951050,
             0.05464350,
             0.06501275,
             0.12643400,
             0.15508900,
             0.18232800,
             0.20793550,
             0.2355430)

optim(c(300, 50), lower=c(150, 20), upper=c(400, 100), method='L-BFGS-B', function(x) {
  sum((arena.m - cvd_prob_10_year_male_framingham(age, tot_chol=x[1], hdl_chol=x[2]))^2) })

optim(c(300, 50), lower=c(150, 20), upper=c(400, 100), method='L-BFGS-B', function(x) {
  sum((arena.f - cvd_prob_10_year_female_framingham(age, tot_chol=x[1], hdl_chol=x[2]))^2) })

