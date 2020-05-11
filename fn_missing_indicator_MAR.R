simulate_onerun <- function(n = 10000, # sample size
                            pi_U = 0.5, # P[U = 1] (U binary)
                            alpha_0 = 0, # intercept in A model
                            alpha_U = 0, # Effect of U on A
                            sigma_A = 1, # sd of continuous A
                            rA_equals_U = TRUE, # simply set rA to U if true
                            beta_0 = 0, # interecept in rA model
                            beta_U = 0, # effect of A on rA
                            beta_A = 0, # effect of U on rA
                            beta_UA = 0, # interaction effect: A and U on rA
                            gamma_0 = 0, # intercept in Y model
                            gamma_U = 0, # effect of U on Y
                            gamma_A = 0, # effect of A on Y
                            gamma_UA = 0, # interaction effect: A and U on Y
                            sigma_Y = 1 # sd of y given A and U 
                            ){

  ##generates 'true' data from structural models
  df <- tibble(
    U = as.numeric(runif(n) < pi_U),
    A = rnorm(n, mean = alpha_0 + alpha_U * U, sd = sigma_A),
    rA = if (rA_equals_U) {
      U
    } else {
      as.numeric(runif(n) < expit(beta_0 + beta_U * U + beta_A * A + beta_UA * A * U))
    },
    A_star = ifelse(rA, NA, A),
    y = rnorm(
      n,
      mean = gamma_0 + gamma_A * A + gamma_U * U + gamma_UA * A * U,
      sd = sigma_Y
    )
  )

  # run imputation models (fixed and stochastic)
  unimp <- df %>% select(rA, A_star, y)
  unimp_MAR <- df %>% select(rA, U, A_star, y) %>% 
    mutate(Uy_int = U*y)
 # umean_imp <- mice(unimp, method = "mean", m = 1, maxit = 1, print = FALSE)
#  fixedreg_imp <- mice(unimp, method = "norm.predict", m = 1, maxit = 1, print = FALSE)
  
  # uses norm (Bayesian linear regression) - much faster than pmm -- note that this is proper in the sense of Rubin
  # and fine as A is normally distributed.
  # only one iteration needed as only A has missing data.
  stochastic_imp <- mice(unimp, method = "norm", maxit = 1, print = FALSE)
  stochastic_imp_MAR <- mice(unimp_MAR, method = "norm", maxit = 1, print = FALSE)
  
  nomiss <- lm(y ~ A + U, data = df)
  nomiss_interact <- lm(y ~ A * U, data = df)
  nomiss_noR <- lm(y ~ A, data = df)
  
  complete_case_MAR <- lm(y ~ A_star + U, data = df)
  complete_case_MAR_interact <- lm(y ~ A_star * U, data = df)
  complete_case <- lm(y ~ A_star, data = df)
  
  stochastic <- with(stochastic_imp, lm(y ~ A_star + rA))
  stochastic_interact <- with(stochastic_imp, lm(y ~ A_star * rA))
  stochastic_noR <- with(stochastic_imp, lm(y ~ A_star))
  
  stochastic_MAR <- with(stochastic_imp_MAR, lm(y ~ A_star + rA + U + A_star:U))
  stochastic_MAR_interact <- with(stochastic_imp_MAR, lm(y ~ A_star + rA + A_star:rA  + U + A_star:U))
  stochastic_MAR_noR <- with(stochastic_imp_MAR, lm(y ~ A_star  + U + A_star:U))
  
  # fixedreg <- with(fixedreg_imp, lm(y ~ A_star + rA))
  # fixedreg_interact <- with(fixedreg_imp, lm(y ~ A_star * rA))
  # fixedreg_noR <- with(fixedreg_imp, lm(y ~ A_star))
  # umean <- with(umean_imp, lm(y ~ A_star + rA))
  # umean_interact <- with(umean_imp, lm(y ~ A_star * rA))
  # umean_noR <- with(umean_imp, lm(y ~ A_star))
  
  
  
  model_outputs <- list()
  model_outputs$nomiss <- nomiss %>% summary %>% coef
  model_outputs$nomiss_interact <- nomiss_interact %>% summary %>% coef
  model_outputs$nomiss_noR <- nomiss_noR %>% summary %>% coef
  model_outputs$complete_case_MAR <- complete_case_MAR %>% summary %>% coef
  model_outputs$complete_case_MAR_interact <- complete_case_MAR_interact %>% summary %>% coef
  model_outputs$complete_case <- complete_case %>% summary %>% coef
  model_outputs$stochastic <- stochastic %>% pool %>% summary
  model_outputs$stochastic_interact <- 
    stochastic_interact %>% pool %>% summary
  model_outputs$stochastic_noR <- stochastic_noR %>% pool %>% summary
  model_outputs$stochastic_MAR <- stochastic_MAR %>% pool %>% summary
  model_outputs$stochastic_MAR_interact <- 
    stochastic_MAR_interact %>% pool %>% summary
  model_outputs$stochastic_MAR_noR <- stochastic_MAR_noR %>% pool %>% summary
  # model_outputs$fixedreg <- fixedreg %>% summary
  # model_outputs$fixedreg_interact <- 
  #   fixedreg_interact %>% summary
  # model_outputs$fixedreg_noR <- fixedreg_noR %>% summary
  # model_outputs$umean <- umean %>% summary
  # model_outputs$umean_interact <- 
  #   umean_interact %>% summary
  # model_outputs$umean_noR <- umean_noR %>% summary
  
  
  model_outputs$nomiss_MSE <- extract_mse_lm(nomiss)
  model_outputs$nomiss_interact_MSE <- extract_mse_lm(nomiss_interact)
  model_outputs$nomiss_noR_MSE <- extract_mse_lm(nomiss_noR)
  model_outputs$complete_case_MAR_MSE <- extract_mse_lm(complete_case_MAR)
  model_outputs$complete_case_MAR_interact_MSE <- extract_mse_lm(complete_case_MAR_interact)
  model_outputs$complete_case_MSE <- extract_mse_lm(complete_case)
  model_outputs$stochastic_MSE <- extract_mse_mice(stochastic)
  model_outputs$stochastic_interact_MSE <- extract_mse_mice(stochastic_interact)
  model_outputs$stochastic_noR_MSE <- extract_mse_mice(stochastic_noR)
  model_outputs$stochastic_MAR_MSE <- extract_mse_mice(stochastic_MAR)
  model_outputs$stochastic_MAR_interact_MSE <- extract_mse_mice(stochastic_MAR_interact)
  model_outputs$stochastic_MAR_noR_MSE <- extract_mse_mice(stochastic_MAR_noR)
  # model_outputs$fixedreg_MSE <- extract_mse_mice(fixedreg)
  # model_outputs$fixedreg_interact_MSE <- extract_mse_mice(fixedreg_interact)
  # model_outputs$fixedreg_noR_MSE <- extract_mse_mice(fixedreg_noR)
  # model_outputs$umean_MSE <- extract_mse_mice(umean)
  # model_outputs$umean_interact_MSE <- extract_mse_mice(umean_interact)
  # model_outputs$umean_noR_MSE <- extract_mse_mice(umean_noR)
  
  
  model_outputs
}


expit <- function(x) {
  1 / (1 + exp(-x))
}



simulate_nrun <- function(n = 10000, # sample size
                          pi_U = 0.5, # P[U = 1] (U binary)
                          alpha_0 = 0, # intercept in A model
                          alpha_U = 0, # Effect of U on A
                          sigma_A = 1, # sd of continuous A
                          rA_equals_U = TRUE, # simply set rA to U if true
                          beta_0 = 0, # intercept in rA model
                          beta_U = 0, # effect of A on rA
                          beta_A = 0, # effect of U on rA
                          beta_UA = 0, # interaction effect: A and U on rA
                          gamma_0 = 0, # intercept in Y model
                          gamma_U = 0, # effect of U on Y
                          gamma_A = 0, # effect of A on Y
                          gamma_UA = 0, # interaction effect: A and U on Y
                          sigma_Y = 1, # sd of y given A and U
                          n_rep = 100 #number of times to repeat.
){
  # a wrapper for main above functions that extracts the info needed
  # for one set of the simulation parameters.
  # extract each of the 'gammas' referred to in the paper.
  
  res <- matrix(nrow = n_rep, ncol = 72)
  colnames(res) <- c("gamma_0_1N", "gamma_A_1N", 
                     "gamma_0_2N", "gamma_A_2N", 
                     "gamma_0_3N", "gamma_A_3N", 
                     "gamma_0_1C", "gamma_A_1C", 
                     "gamma_0_2C", "gamma_A_2C",  
                     "gamma_0_3C", "gamma_A_3C", 
                     "gamma_0_1S", "gamma_A_1S", 
                     "gamma_0_2S", "gamma_A_2S", "gamma_R_2S", 
                     "gamma_0_3S", "gamma_A_3S", "gamma_R_3S", "gamma_RA_3S",
                     "gamma_0_1M", "gamma_A_1M", 
                     "gamma_0_2M", "gamma_A_2M", "gamma_R_2M", 
                     "gamma_0_3M", "gamma_A_3M", "gamma_R_3M", "gamma_RA_3M",
                     "se_gamma_0_1N", "se_gamma_A_1N", 
                     "se_gamma_0_2N", "se_gamma_A_2N", 
                     "se_gamma_0_3N", "se_gamma_A_3N", 
                     "se_gamma_0_1C", "se_gamma_A_1C", 
                     "se_gamma_0_2C", "se_gamma_A_2C",  
                     "se_gamma_0_3C", "se_gamma_A_3C", 
                     "se_gamma_0_1S", "se_gamma_A_1S", 
                     "se_gamma_0_2S", "se_gamma_A_2S", "se_gamma_R_2S", 
                     "se_gamma_0_3S", "se_gamma_A_3S", "se_gamma_R_3S", "se_gamma_RA_3S",
                     "se_gamma_0_1M", "se_gamma_A_1M", 
                     "se_gamma_0_2M", "se_gamma_A_2M", "se_gamma_R_2M", 
                     "se_gamma_0_3M", "se_gamma_A_3M", "se_gamma_R_3M", "se_gamma_RA_3M",
                     "mse_1N", "mse_2N", "mse_3N",
                    "mse_1C", "mse_2C", "mse_3C", "mse_1S", "mse_2S", "mse_3S", 
                     "mse_1M", "mse_2M", "mse_3M")
  
  for (i in 1:n_rep){
    res_full <- simulate_onerun(n, pi_U, alpha_0, alpha_U, sigma_A, rA_equals_U, beta_0, beta_U, 
                                beta_A, beta_UA, gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y)
    # browser()
    res[i,1:2] <- res_full$nomiss_noR[,1]
    res[i,3:4] <- res_full$nomiss[1:2,1]
    res[i,5:6] <- res_full$nomiss_interact[1:2,1]
    res[i,7:8] <- res_full$complete_case[,1]
    res[i,9:10] <- res_full$complete_case_MAR[1:2,1]
    res[i,11:12] <- res_full$complete_case_MAR_interact[1:2,1]
    res[i,13:14] <- res_full$stochastic_noR$estimate
    res[i,15:17] <- res_full$stochastic$estimate
    res[i,18:21] <- res_full$stochastic_interact$estimate
    res[i,22:23] <- res_full$stochastic_MAR_noR$estimate[1:2]
    res[i,24:26] <- res_full$stochastic_MAR$estimate[1:3]
    
    if(length(res_full$stochastic_MAR_interact$estimate) == 6)
      res[i,27:30] <- res_full$stochastic_MAR_interact$estimate[c(1,2,3,5)]
    else
      res[i,27:30] <- res_full$stochastic_MAR_interact$estimate[c(1,2,3,4)]
    
    res[i,31:32] <- res_full$nomiss_noR[,2]
    res[i,33:34] <- res_full$nomiss[1:2,2]
    res[i,35:36] <- res_full$nomiss_interact[1:2,2]
    res[i,37:38] <- res_full$complete_case[,2]
    res[i,39:40] <- res_full$complete_case_MAR[1:2,2]
    res[i,41:42] <- res_full$complete_case_MAR_interact[1:2,2]
    res[i,43:44] <- res_full$stochastic_noR$std.error
    res[i,45:47] <- res_full$stochastic$std.error
    res[i,48:51] <- res_full$stochastic_interact$std.error
    res[i,52:53] <- res_full$stochastic_MAR_noR$std.error[1:2]
    res[i,54:56] <- res_full$stochastic_MAR$std.error[1:3]
    
    res[i,57:60] <- res_full$stochastic_MAR_interact$std.error[1:4]
    
    if(length(res_full$stochastic_MAR_interact$estimate) == 6)
      res[i,57:60] <- res_full$stochastic_MAR_interact$std.error[c(1,2,3,5)]
    else
      res[i,57:60] <- res_full$stochastic_MAR_interact$std.error[c(1,2,3,4)]
    
    res[i,61] <- res_full$nomiss_noR_MSE
    res[i,62] <- res_full$nomiss_MSE
    res[i,63] <- res_full$nomiss_interact_MSE
    res[i,64] <- res_full$complete_case_MSE
    res[i,65] <- res_full$complete_case_MSE
    res[i,66] <- res_full$complete_case_MSE
    res[i,67] <- res_full$stochastic_noR_MSE
    res[i,68] <- res_full$stochastic_MSE
    res[i,69] <- res_full$stochastic_interact_MSE
    res[i,70] <- res_full$stochastic_MAR_noR_MSE
    res[i,71] <- res_full$stochastic_MAR_MSE
    res[i,72] <- res_full$stochastic_MAR_interact_MSE

    }

  res
}




extract_mse_mice <- function(micemod){
  # takes the result of a mice fit and calculates mse
  map(micemod$analyses, ~`$`(., "residuals") %>% `^`(2) %>% mean) %>% 
    as.numeric %>% mean
}

extract_mse_lm <- function(lmmod){
  mean(lmmod$residuals^2)
}

