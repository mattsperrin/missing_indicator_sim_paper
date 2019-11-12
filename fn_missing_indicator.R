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
  umean_imp <- mice(unimp, method = "mean", m = 1, maxit = 1, print = FALSE)
  fixedreg_imp <- mice(unimp, method = "norm.predict", m = 1, maxit = 1, print = FALSE)
  
  # uses norm (Bayesian linear regression) - much faster than pmm
  # and fine as A is normally distributed.
  # only one iteration needed as only A has missing data.
  stochastic_imp <- mice(unimp, method = "norm", maxit = 1, print = FALSE)
  
  nomiss <- lm(y ~ A + U, data = df)
  nomiss_interact <- lm(y ~ A * U, data = df)
  nomiss_noR <- lm(y ~ A, data = df)
  complete_case <- lm(y ~ A_star, data = df)
  stochastic <- with(stochastic_imp, lm(y ~ A_star + rA))
  stochastic_interact <- with(stochastic_imp, lm(y ~ A_star * rA))
  stochastic_noR <- with(stochastic_imp, lm(y ~ A_star))
  fixedreg <- with(fixedreg_imp, lm(y ~ A_star + rA))
  fixedreg_interact <- with(fixedreg_imp, lm(y ~ A_star * rA))
  fixedreg_noR <- with(fixedreg_imp, lm(y ~ A_star))
  umean <- with(umean_imp, lm(y ~ A_star + rA))
  umean_interact <- with(umean_imp, lm(y ~ A_star * rA))
  umean_noR <- with(umean_imp, lm(y ~ A_star))
  
  model_outputs <- list()
  model_outputs$nomiss <- nomiss %>% summary %>% coef
  model_outputs$nomiss_interact <- nomiss_interact %>% summary %>% coef
  model_outputs$nomiss_noR <- nomiss_noR %>% summary %>% coef
  model_outputs$complete_case <- complete_case %>% summary %>% coef
  model_outputs$stochastic <- stochastic %>% pool %>% summary
  model_outputs$stochastic_interact <- 
    stochastic_interact %>% pool %>% summary
  model_outputs$stochastic_noR <- stochastic_noR %>% pool %>% summary
  model_outputs$fixedreg <- fixedreg %>% summary
  model_outputs$fixedreg_interact <- 
    fixedreg_interact %>% summary
  model_outputs$fixedreg_noR <- fixedreg_noR %>% summary
  model_outputs$umean <- umean %>% summary
  model_outputs$umean_interact <- 
    umean_interact %>% summary
  model_outputs$umean_noR <- umean_noR %>% summary
  
  
  model_outputs$nomiss_MSE <- extract_mse_lm(nomiss)
  model_outputs$nomiss_interact_MSE <- extract_mse_lm(nomiss_interact)
  model_outputs$nomiss_noR_MSE <- extract_mse_lm(nomiss_noR)
  model_outputs$complete_case_MSE <- extract_mse_lm(complete_case)
  model_outputs$stochastic_MSE <- extract_mse_mice(stochastic)
  model_outputs$stochastic_interact_MSE <- extract_mse_mice(stochastic_interact)
  model_outputs$stochastic_noR_MSE <- extract_mse_mice(stochastic_noR)
  model_outputs$fixedreg_MSE <- extract_mse_mice(fixedreg)
  model_outputs$fixedreg_interact_MSE <- extract_mse_mice(fixedreg_interact)
  model_outputs$fixedreg_noR_MSE <- extract_mse_mice(fixedreg_noR)
  model_outputs$umean_MSE <- extract_mse_mice(umean)
  model_outputs$umean_interact_MSE <- extract_mse_mice(umean_interact)
  model_outputs$umean_noR_MSE <- extract_mse_mice(umean_noR)
  
  
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
  
  res <- matrix(nrow = n_rep, ncol = 89)
  colnames(res) <- c("gamma_0_1N", "gamma_A_1N", 
                     "gamma_0_2N", "gamma_A_2N", "gamma_R_2N", 
                     "gamma_0_3N", "gamma_A_3N", "gamma_R_3N", "gamma_RA_3N",
                     "gamma_0_0", "gamma_A_0",  
                     "gamma_0_1", "gamma_A_1", 
                     "gamma_0_2", "gamma_A_2", "gamma_R_2", 
                     "gamma_0_3", "gamma_A_3", "gamma_R_3", "gamma_RA_3",
                     "gamma_0_1F", "gamma_A_1F", 
                     "gamma_0_2F", "gamma_A_2F", "gamma_R_2F", 
                     "gamma_0_3F", "gamma_A_3F", "gamma_R_3F", "gamma_RA_3F",
                     "gamma_0_1U", "gamma_A_1U", 
                     "gamma_0_2U", "gamma_A_2U", "gamma_R_2U", 
                     "gamma_0_3U", "gamma_A_3U", "gamma_R_3U", "gamma_RA_3U",
                     "se_gamma_0_1N", "se_gamma_A_1N", 
                     "se_gamma_0_2N", "se_gamma_A_2N", "se_gamma_R_2N", 
                     "se_gamma_0_3N", "se_gamma_A_3N", "se_gamma_R_3N", "se_gamma_RA_3N",
                     "se_gamma_0_0", "se_gamma_A_0",  
                     "se_gamma_0_1", "se_gamma_A_1", 
                     "se_gamma_0_2", "se_gamma_A_2", "se_gamma_R_2", 
                     "se_gamma_0_3", "se_gamma_A_3", "se_gamma_R_3", "se_gamma_RA_3",
                     "se_gamma_0_1F", "se_gamma_A_1F", 
                     "se_gamma_0_2F", "se_gamma_A_2F", "se_gamma_R_2F", 
                     "se_gamma_0_3F", "se_gamma_A_3F", "se_gamma_R_3F", "se_gamma_RA_3F",
                     "se_gamma_0_1U", "se_gamma_A_1U", 
                     "se_gamma_0_2U", "se_gamma_A_2U", "se_gamma_R_2U", 
                     "se_gamma_0_3U", "se_gamma_A_3U", "se_gamma_R_3U", "se_gamma_RA_3U",
                     "mse_1N", "mse_2N", "mse_3N",
                     "mse_0", "mse_1", "mse_2", "mse_3", "mse_1F", "mse_2F", "mse_3F", 
                     "mse_1U", "mse_2U", "mse_3U")
  
  for(i in 1:n_rep){
    res_full <- simulate_onerun(n, pi_U, alpha_0, alpha_U, sigma_A, rA_equals_U, beta_0, beta_U, 
                                beta_A, beta_UA, gamma_0, gamma_U, gamma_A, gamma_UA, sigma_Y)
    #browser()
    res[i,1:2] <- res_full$nomiss_noR[,1]
    res[i,3:5] <- res_full$nomiss[,1]
    res[i,6:9] <- res_full$nomiss_interact[,1]
    res[i,10:11] <- res_full$complete_case[,1]
    res[i,12:13] <- res_full$stochastic_noR$estimate
    res[i,14:16] <- res_full$stochastic$estimate
    res[i,17:20] <- res_full$stochastic_interact$estimate
    res[i,21:22] <- res_full$fixedreg_noR$estimate
    res[i,23:25] <- res_full$fixedreg$estimate
    res[i,26:29] <- res_full$fixedreg_interact$estimate[1:4] # interaction might be NA
    res[i,30:31] <- res_full$umean_noR$estimate
    res[i,32:34] <- res_full$umean$estimate
    res[i,35:38] <- res_full$umean_interact$estimate[1:4] # interaction might be NA
    
    res[i,39:40] <- res_full$nomiss_noR[,2]
    res[i,41:43] <- res_full$nomiss[,2]
    res[i,44:47] <- res_full$nomiss_interact[,2]
    res[i,48:49] <- res_full$complete_case[,2]
    res[i,50:51] <- res_full$stochastic_noR$std.error
    res[i,52:54] <- res_full$stochastic$std.error
    res[i,55:58] <- res_full$stochastic_interact$std.error
    res[i,59:60] <- res_full$fixedreg_noR$std.error
    res[i,61:63] <- res_full$fixedreg$std.error
    res[i,64:67] <- res_full$fixedreg_interact$std.error[1:4] # interaction might be NA  
    res[i,68:69] <- res_full$umean_noR$std.error
    res[i,70:72] <- res_full$umean$std.error
    res[i,73:76] <- res_full$umean_interact$std.error[1:4] # interaction might be NA  
    
    res[i,77] <- res_full$nomiss_noR_MSE
    res[i,78] <- res_full$nomiss_MSE
    res[i,79] <- res_full$nomiss_interact_MSE
    res[i,80] <- res_full$complete_case_MSE
    res[i,81] <- res_full$stochastic_noR_MSE
    res[i,82] <- res_full$stochastic_MSE
    res[i,83] <- res_full$stochastic_interact_MSE
    res[i,84] <- res_full$fixedreg_noR_MSE
    res[i,85] <- res_full$fixedreg_MSE
    res[i,86] <- res_full$fixedreg_interact_MSE
    res[i,87] <- res_full$umean_noR_MSE
    res[i,88] <- res_full$umean_MSE
    res[i,89] <- res_full$umean_interact_MSE
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

