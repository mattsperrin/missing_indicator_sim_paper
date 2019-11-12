source("code/fn_missing_indicator.R")
library(tidyverse)
library(mice)
library(furrr)

# standard grids for parameters
grd4 <- c(0, 0.1, 0.5, 1)
grdint <- c(0, 0.5)


# fixing sigmaA = 1 and gammaA = 1 for computational reasons

# main cases: rA not equal U
sims_rA_neq_U <- crossing(
  n = 10000,
  # sample size
  pi_U = 0.5,
  # P[U = 1] (U binary)
  p_rA = c(0.25, 0.5, 0.75),
  # P[rA = 1] ie proportion missing
  alpha_0 = 0,
  # intercept in A model
  alpha_U = grd4,
  # Effect of U on A
  sigma_A = 1, #grd4[-1],
  # sd of continuous A
  rA_equals_U = FALSE,
  # simply set rA to U if true
  beta_0 = 0,
  # interecept in rA model
  beta_U = grd4,
  # effect of A on rA
  beta_A = grd4,
  # effect of U on rA
  beta_UA = grdint,
  # interaction effect: A and U on rA
  gamma_0 = 0,
  # intercept in Y model
  gamma_U = grd4,
  # effect of U on Y
  gamma_A = 1,
  # effect of A on Y
  gamma_UA = grdint,
  # interaction effect: A and U on Y
  sigma_Y = grd4[-1]
) # sd of y given A and U

# correct intercepts so that EA = EY = 0 and ErA = p_rA:
sims_rA_neq_U <- sims_rA_neq_U %>% 
  mutate(
    alpha_0 = -pi_U * alpha_U,
    beta_0 = -pi_U * beta_U + log(p_rA / (1 - p_rA)),
    gamma_0 = -pi_U * gamma_U
  )

# cases where rA = U:
sims_rA_eq_U <- crossing(
  n = 10000,
  # sample size
  pi_U = c(0.25, 0.5, 0.75),
  # P[U = 1] (U binary)
  p_rA = 0.5,
  # P[rA = 1] ie proportion missing
  alpha_0 = 0,
  # intercept in A model
  alpha_U = grd4,
  # Effect of U on A
  sigma_A = 1, #grd4[-1],
  # sd of continuous A
  rA_equals_U = TRUE,
  # simply set rA to U if true
  beta_0 = 0,
  # interecept in rA model
  beta_U = 0,
  # effect of A on rA
  beta_A = 0,
  # effect of U on rA
  beta_UA = 0,
  # interaction effect: A and U on rA
  gamma_0 = 0,
  # intercept in Y model
  gamma_U = grd4,
  # effect of U on Y
  gamma_A = 1, #grd4,
  # effect of A on Y
  gamma_UA = grdint,
  # interaction effect: A and U on Y
  sigma_Y = grd4[-1]
) # sd of y given A and U 

# correct intercepts so that EA = EY = 0 and ErA = p_rA:
sims_rA_eq_U <- sims_rA_eq_U %>% 
  mutate(
    alpha_0 = -pi_U * alpha_U,
    beta_0 = -pi_U * beta_U + log(p_rA / (1 - p_rA)),
    gamma_0 = -pi_U * gamma_U
  )


# number of repeats per scenario
n_rep <- 200



# the following lines are expected to take around 30 hours to run:

plan(multiprocess)

set.seed(133290719)
sims_rA_eq_U <- sims_rA_eq_U %>% 
  mutate(results = future_pmap(list(n, pi_U, alpha_0, alpha_U, sigma_A, 
                             rA_equals_U, beta_0, beta_U, beta_A, beta_UA, 
                             gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y, n_rep),
                        simulate_nrun))


saveRDS(sims_rA_eq_U, file = "output/sims_rA_eq_U.RDS")

set.seed(133290720)

sims_rA_neq_U <- sims_rA_neq_U %>% 
  mutate(results = future_pmap(list(n, pi_U, alpha_0, alpha_U, sigma_A, 
                             rA_equals_U, beta_0, beta_U, beta_A, beta_UA, 
                             gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y, n_rep),
                        simulate_nrun))

saveRDS(sims_rA_neq_U, file = "output/sims_rA_neq_U.RDS")




# # test doing 10 only:
# 
#  system.time({ 
#   sims_rA_eq_U1 <- sims_rA_eq_U[1:10,] %>%
#     mutate(results = pmap(list(n, pi_U, alpha_0, alpha_U, sigma_A,
#                                rA_equals_U, beta_0, beta_U, beta_A, beta_UA,
#                                gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y, n_rep),
#                           simulate_nrun))
#  })
# 
#  system.time({ 
#    sims_rA_eq_U2 <- sims_rA_eq_U[1:10,] %>%
#      mutate(results = future_pmap(list(n, pi_U, alpha_0, alpha_U, sigma_A,
#                                 rA_equals_U, beta_0, beta_U, beta_A, beta_UA,
#                                 gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y, n_rep),
#                            simulate_nrun))
#  })
#  
#  
#  system.time({ 
#    plan(multiprocess)
#    sims_rA_eq_U3 <- sims_rA_eq_U[1:10,] %>%
#      mutate(results = future_pmap(list(n, pi_U, alpha_0, alpha_U, sigma_A,
#                                        rA_equals_U, beta_0, beta_U, beta_A, beta_UA,
#                                        gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y, n_rep),
#                                   simulate_nrun))
#  })
 