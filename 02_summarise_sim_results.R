library(tidyverse)
sims_rA_eq_U <- readRDS("output/sims_rA_eq_U.RDS")
sims_rA_neq_U <- readRDS("output/sims_rA_neq_U.RDS")
sims <- rbind(sims_rA_eq_U, sims_rA_neq_U)

# summarise results
sims$results <- map(sims$results, as_tibble)

sims_summary <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(gamma_A_0, gamma_A_1, gamma_A_2, gamma_R_2, 
                    gamma_A_3, gamma_R_3, gamma_RA_3,
                    gamma_A_1F, gamma_A_2F, gamma_R_2F, 
                    gamma_A_3F, gamma_R_3F, gamma_RA_3F,
                    gamma_A_1U, gamma_A_2U, gamma_R_2U, 
                    gamma_A_3U, gamma_R_3U, gamma_RA_3U,
                    mse_0, mse_1, mse_2, mse_3, mse_1F, mse_2F, mse_3F, 
                    mse_1U, mse_2U, mse_3U), 
               funs(med = median(., na.rm = TRUE), 
                    low = quantile(., probs = 0.025, na.rm = TRUE),
                    LQ = quantile(., probs = 0.25, na.rm = TRUE),
                    UQ = quantile(., probs = 0.75, na.rm = TRUE),
                    high = quantile(., probs = 0.975, na.rm = TRUE)
                    
               )) %>% 
  ungroup


sims_summary_SE <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(se_gamma_A_0, se_gamma_A_1, se_gamma_A_2, se_gamma_R_2, 
                    se_gamma_A_3, se_gamma_R_3, se_gamma_RA_3,
                    se_gamma_A_1F, se_gamma_A_2F, se_gamma_R_2F, 
                    se_gamma_A_3F, se_gamma_R_3F, se_gamma_RA_3F,
                    se_gamma_A_1U, se_gamma_A_2U, se_gamma_R_2U, 
                    se_gamma_A_3U, se_gamma_R_3U, se_gamma_RA_3U
                    ), 
               funs(mean = mean
               )) %>% 
  ungroup

sims_summary_empSE <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(gamma_A_0, gamma_A_1, gamma_A_2, gamma_R_2, 
                    gamma_A_3, gamma_R_3, gamma_RA_3,
                    gamma_A_1F, gamma_A_2F, gamma_R_2F, 
                    gamma_A_3F, gamma_R_3F, gamma_RA_3F,
                    gamma_A_1U, gamma_A_2U, gamma_R_2U, 
                    gamma_A_3U, gamma_R_3U, gamma_RA_3U), 
               funs(se = sd
               )) %>% 
  ungroup

sims_summary_SE <- sims_summary_SE %>% 
  left_join(sims_summary_empSE)




write.csv(sims_summary, file = "output/sims_summary.csv")
write.csv(sims_summary_SE, file = "output/sims_summary_SE.csv")


# organise for plotting
sims_summary_gathered <- sims_summary %>% 
  gather(key = estimate_type, value = value, gamma_A_0_med:mse_3U_high) %>% 
  separate(estimate_type, into = c("estimate", "type"), sep = "_(?!.*_)") %>% 
  spread(key = type, value = value) %>% 
  separate(estimate, into = c("parameter", "model"), sep = "_(?=[:digit:])", extra = "drop")


sims_summary_gathered$model <- ifelse(is.na(sims_summary_gathered$model), 9, sims_summary_gathered$model)

write_csv(sims_summary_gathered, "output/sims_summary_gathered.csv")

colnames(sims_summary_SE) <- str_remove(colnames(sims_summary_SE), "se_")

sims_summary_SE_gathered <- sims_summary_SE %>% 
  gather(key = estimate_type, value = value, gamma_A_0_mean:gamma_RA_3U_se) %>% 
  separate(estimate_type, into = c("estimate", "type"), sep = "_(?!.*_)") %>% 
  spread(key = type, value = value) %>% 
  separate(estimate, into = c("parameter", "model"), sep = "_(?=[:digit:])", extra = "drop") %>% 
  rename("Average Model Based" = mean, "Empirical" = se) %>% 
  gather(key = "SE type", value = value, `Average Model Based`:`Empirical`)
  

sims_summary_SE_gathered$model <- ifelse(is.na(sims_summary_SE_gathered$model), 9, sims_summary_SE_gathered$model)

write_csv(sims_summary_SE_gathered, "output/sims_summary_SE_gathered.csv")
  