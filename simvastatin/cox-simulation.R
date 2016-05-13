# "General Cardiovascular Risk Profile for Use in Primary Care", Ralph D'Agostino, 2007


# Table 1,  and Table 2
t_f  <- 10 # 10 years

so_f <- 0.95012
n_f  <- 8491 - 4522
xi_f <- c(log(49.1), log(215.1), log(57.6), log(125.8)*(1-532/n_f), log(125.8)*532/n_f, 1548/n_f, 170/n_f )
beta <- c(2.32888, 1.20904, -0.70833, 2.76157, 2.82263, 0.52873, 0.69154)

center <- sum(beta * xi_f)

# Appendix, Case 1

x1_f <- c(log(61), log(180), log(47), log(124), 0, 1, 0)

risk <- sum(x1_f*beta) - center

prob <- 1 - so_f^exp(risk)

rate <- -log(1-prob)*rr/t_f

x <- rexp(10000, rate)

print(length(x[x < 10]) / 10000)

# Now ignore all variables but age (since it's centered)

xi_f <- log(49.1)
beta <- 2.32888
center <- sum(beta * xi_f)

x1_f <- log(80)

risk <- sum(x1_f*beta) - center

prob <- 1 - so_f^exp(risk)

