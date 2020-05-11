library(tidyverse)
sims_rA_eq_U <- readRDS("output/sims_rA_eq_U.RDS")
sims_rA_neq_U <- readRDS("output/sims_rA_neq_U.RDS")
sims <- rbind(sims_rA_eq_U, sims_rA_neq_U)

# summarise results
sims$results <- map(sims$results, as_tibble)

sims_summary <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(gamma_A_1N, gamma_A_2N, 
                    gamma_A_3N, 
                    gamma_A_1C, gamma_A_2C,  
                    gamma_A_3C,
                    gamma_A_1S, gamma_A_2S, gamma_R_2S, 
                    gamma_A_3S, gamma_R_3S, gamma_RA_3S,
                    gamma_A_1M, gamma_A_2M, gamma_R_2M, 
                    gamma_A_3M, gamma_R_3M, gamma_RA_3M,
                    mse_1N, mse_2N, mse_3N, mse_1C, mse_2C, mse_3C, 
                    mse_1S, mse_2S, mse_3S, mse_1M, mse_2M, mse_3M), 
               funs(med = median(., na.rm = TRUE),
                    mean = mean(., na.rm = TRUE),
                    low = quantile(., probs = 0.025, na.rm = TRUE),
                    LQ = quantile(., probs = 0.25, na.rm = TRUE),
                    UQ = quantile(., probs = 0.75, na.rm = TRUE),
                    high = quantile(., probs = 0.975, na.rm = TRUE)
                    
               )) %>% 
  ungroup


sims_summary_SE <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(se_gamma_A_1N, se_gamma_A_2N, 
                    se_gamma_A_3N, 
                    se_gamma_A_1C, se_gamma_A_2C, 
                    se_gamma_A_3C, 
                    se_gamma_A_1S, se_gamma_A_2S, se_gamma_R_2S, 
                    se_gamma_A_3S, se_gamma_R_3S, se_gamma_RA_3S,
                    se_gamma_A_1M, se_gamma_A_2M, se_gamma_R_2M, 
                    se_gamma_A_3M, se_gamma_R_3M, se_gamma_RA_3M
                    ), 
               funs(mean = mean
               )) %>% 
  ungroup

sims_summary_empSE <- sims %>% 
  unnest %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(gamma_A_1N, gamma_A_2N, 
                    gamma_A_3N, 
                    gamma_A_1C, gamma_A_2C, 
                    gamma_A_3C, 
                    gamma_A_1S, gamma_A_2S, gamma_R_2S, 
                    gamma_A_3S, gamma_R_3S, gamma_RA_3S,
                    gamma_A_1M, gamma_A_2M, gamma_R_2M, 
                    gamma_A_3M, gamma_R_3M, gamma_RA_3M), 
               funs(se = sd
               )) %>% 
  ungroup

sims_summary_SE <- sims_summary_SE %>% 
  left_join(sims_summary_empSE)


sims_summary_coverage <- sims %>% 
  unnest %>% 
  mutate(cover_1N = ((gamma_A_1N - 1.96 * se_gamma_A_1N) < gamma_A & (gamma_A_1N + 1.96 * se_gamma_A_1N) > gamma_A) * 1,
         cover_1C = ((gamma_A_1C - 1.96 * se_gamma_A_1C) < gamma_A & (gamma_A_1C + 1.96 * se_gamma_A_1C) > gamma_A) * 1,
         cover_1S = ((gamma_A_1S - 1.96 * se_gamma_A_1S) < gamma_A & (gamma_A_1S + 1.96 * se_gamma_A_1S) > gamma_A) * 1,
         cover_1M = ((gamma_A_1M - 1.96 * se_gamma_A_1M) < gamma_A & (gamma_A_1M + 1.96 * se_gamma_A_1M) > gamma_A) * 1,
         cover_2N = ((gamma_A_2N - 1.96 * se_gamma_A_2N) < gamma_A & (gamma_A_2N + 1.96 * se_gamma_A_2N) > gamma_A) * 1,
         cover_2C = ((gamma_A_2C - 1.96 * se_gamma_A_2C) < gamma_A & (gamma_A_2C + 1.96 * se_gamma_A_2C) > gamma_A) * 1,
         cover_2S = ((gamma_A_2S - 1.96 * se_gamma_A_2S) < gamma_A & (gamma_A_2S + 1.96 * se_gamma_A_2S) > gamma_A) * 1,
         cover_2M = ((gamma_A_2M - 1.96 * se_gamma_A_2M) < gamma_A & (gamma_A_2M + 1.96 * se_gamma_A_2M) > gamma_A) * 1,
         cover_3N = ((gamma_A_3N - 1.96 * se_gamma_A_3N) < gamma_A & (gamma_A_3N + 1.96 * se_gamma_A_3N) > gamma_A) * 1,
         cover_3C = ((gamma_A_3C - 1.96 * se_gamma_A_3C) < gamma_A & (gamma_A_3C + 1.96 * se_gamma_A_3C) > gamma_A) * 1,
         cover_3S = ((gamma_A_3S - 1.96 * se_gamma_A_3S) < gamma_A & (gamma_A_3S + 1.96 * se_gamma_A_3S) > gamma_A) * 1,
         cover_3M = ((gamma_A_3M - 1.96 * se_gamma_A_3M) < gamma_A & (gamma_A_3M + 1.96 * se_gamma_A_3M) > gamma_A) * 1,
         ) %>% 
  group_by_at(vars(n:sigma_Y)) %>% 
  summarise_at(vars(cover_1N, cover_2N, cover_3N, 
                    cover_1C, cover_2C, cover_3C,
                    cover_1S, cover_2S, cover_3S,
                    cover_1M, cover_2M, cover_3M), 
               funs(mn = mean
               )) %>% 
  ungroup
  


write.csv(sims_summary, file = "output/sims_summary.csv")
write.csv(sims_summary_SE, file = "output/sims_summary_SE.csv")
write.csv(sims_summary_coverage, file = "output/sims_summary_coverage.csv")


# organise for plotting
sims_summary_gathered <- sims_summary %>% 
  gather(key = estimate_type, value = value, gamma_A_1N_med:mse_3M_high) %>% 
  separate(estimate_type, into = c("estimate", "type"), sep = "_(?!.*_)") %>% 
  spread(key = type, value = value) %>% 
  separate(estimate, into = c("parameter", "model"), sep = "_(?=[:digit:])", extra = "drop")


sims_summary_gathered$model <- ifelse(is.na(sims_summary_gathered$model), 9, sims_summary_gathered$model)

write_csv(sims_summary_gathered, "output/sims_summary_gathered.csv")

colnames(sims_summary_SE) <- str_remove(colnames(sims_summary_SE), "se_")

sims_summary_SE_gathered <- sims_summary_SE %>% 
  gather(key = estimate_type, value = value, gamma_A_1N_mean:gamma_RA_3M_se) %>% 
  separate(estimate_type, into = c("estimate", "type"), sep = "_(?!.*_)") %>% 
  spread(key = type, value = value) %>% 
  separate(estimate, into = c("parameter", "model"), sep = "_(?=[:digit:])", extra = "drop") %>% 
  rename("Average Model Based" = mean, "Empirical" = se) %>% 
  gather(key = "SE type", value = value, `Average Model Based`:`Empirical`)
  

sims_summary_SE_gathered$model <- ifelse(is.na(sims_summary_SE_gathered$model), 9, sims_summary_SE_gathered$model)

write_csv(sims_summary_SE_gathered, "output/sims_summary_SE_gathered.csv")
  