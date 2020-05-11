library(tidyverse)

sims_summary <- read_csv("../output/sims_summary.csv")
sims_summary_SE <- read_csv("../output/sims_summary_SE.csv")
sims_summary_coverage <- read_csv("../output/sims_summary_coverage.csv")



table_data_MNAR <- sims_summary %>% 
  select(n:sigma_Y, gamma_A_1N_mean, gamma_A_1C_mean, gamma_A_1S_mean, gamma_A_2S_mean, gamma_A_3S_mean
         )


table_data_MAR <- sims_summary %>% 
  select(n:sigma_Y, gamma_A_3N_mean, gamma_A_3C_mean, gamma_A_1M_mean, gamma_A_2M_mean, gamma_A_3M_mean
  )


table_data_SE_MNAR <- sims_summary_SE %>% 
  mutate(gamma_A_1N_width = 3.92*se_gamma_A_1N_mean,
         gamma_A_1C_width = 3.92*se_gamma_A_1C_mean,
         gamma_A_1S_width = 3.92*se_gamma_A_1S_mean,
         gamma_A_2S_width = 3.92*se_gamma_A_2S_mean,
         gamma_A_3S_width = 3.92*se_gamma_A_3S_mean
  ) %>% 
  select(n:sigma_Y, gamma_A_1N_width, gamma_A_1C_width, gamma_A_1S_width, gamma_A_2S_width, gamma_A_3S_width
  )



table_data_SE_MAR <- sims_summary_SE %>% 
  mutate(gamma_A_3N_width = 3.92*se_gamma_A_3N_mean,
         gamma_A_3C_width = 3.92*se_gamma_A_3C_mean,
         gamma_A_1M_width = 3.92*se_gamma_A_1M_mean,
         gamma_A_2M_width = 3.92*se_gamma_A_2M_mean,
         gamma_A_3M_width = 3.92*se_gamma_A_3M_mean
) %>% 
  select(n:sigma_Y, gamma_A_3N_width, gamma_A_3C_width, gamma_A_1M_width, gamma_A_2M_width, gamma_A_3M_width
  )


table_data_coverage_MNAR <- sims_summary_coverage %>% 
  select(n:sigma_Y, cover_1N_mn, cover_1C_mn, cover_1S_mn, cover_2S_mn, cover_3S_mn)


table_data_coverage_MAR <- sims_summary_coverage %>% 
  select(n:sigma_Y, cover_3N_mn, cover_3C_mn, cover_1M_mn, cover_2M_mn, cover_3M_mn)


table_data_MNAR <- table_data_MNAR %>% 
  left_join(table_data_SE_MNAR) %>% 
  left_join(table_data_coverage_MNAR) %>%
  mutate(sim_group = "MNAR")

table_data_MAR <- table_data_MAR %>% 
  left_join(table_data_SE_MAR) %>% 
  left_join(table_data_coverage_MAR) %>% 
  mutate(sim_group = "MAR")





# rename and organise
colnames(table_data_MNAR)

table_data_MNAR <- table_data_MNAR %>% 
  rename(
    CompletedData_Est = gamma_A_1N_mean,
    CompleteCase_Est = gamma_A_1C_mean,
    MI_Est = gamma_A_1S_mean,
    MIMI_Est = gamma_A_2S_mean,
    MIMIInt_Est = gamma_A_3S_mean,
    CompletedData_Width = gamma_A_1N_width,
    CompleteCase_Width = gamma_A_1C_width,
    MI_Width = gamma_A_1S_width,
    MIMI_Width = gamma_A_2S_width,
    MIMIInt_Width = gamma_A_3S_width,
    CompletedData_Cov = cover_1N_mn,
    CompleteCase_Cov = cover_1C_mn,
    MI_Cov = cover_1S_mn,
    MIMI_Cov = cover_2S_mn,
    MIMIInt_Cov = cover_3S_mn,
  ) 


table_data_MAR <- table_data_MAR %>% 
  rename(
    CompletedData_Est = gamma_A_3N_mean,
    CompleteCase_Est = gamma_A_3C_mean,
    MI_Est = gamma_A_1M_mean,
    MIMI_Est = gamma_A_2M_mean,
    MIMIInt_Est = gamma_A_3M_mean,
    CompletedData_Width = gamma_A_3N_width,
    CompleteCase_Width = gamma_A_3C_width,
    MI_Width = gamma_A_1M_width,
    MIMI_Width = gamma_A_2M_width,
    MIMIInt_Width = gamma_A_3M_width,
    CompletedData_Cov = cover_3N_mn,
    CompleteCase_Cov = cover_3C_mn,
    MI_Cov = cover_1M_mn,
    MIMI_Cov = cover_2M_mn,
    MIMIInt_Cov = cover_3M_mn,
  ) 

table_data <- rbind(table_data_MAR, table_data_MNAR) %>% 
  select(n:sigma_Y,
         sim_group,
         starts_with("Completed"),
         starts_with("CompleteC"),
         starts_with("MI_"),
         starts_with("MIMI_"),
         starts_with("MIMII"))


table_data$beta_U[table_data$rA_equals_U] <- 1000




#write_csv(table_data, "output/table_data.csv")


######## individual tables for each of the scenarios corresponding to the figures ####

tabff2 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, sim_group == "MNAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)

tabff3 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1,  sim_group == "MNAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)


tabff4 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)


tabff5 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MNAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)


tabff6 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)

  
tabff7 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MNAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)

  

tabffs1 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MNAR") %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)  



tabffs2 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MNAR")  %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)

tabffs3 <- table_data %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U, sim_group == "MAR")  %>% 
  select(alpha_U, beta_U, beta_A, gamma_UA, sim_group, CompletedData_Est:MIMIInt_Cov)
  
  

all_tables <- list(tabff2, tabff3, tabff4, tabff5, tabff6, tabff7, tabffs1, tabffs2, tabffs3)
names(all_tables) <- list("Table 1 (Fig 2)",
                          "Table 2 (Fig 3)",
                          "Table 3 (Fig 4)",
                          "Table 4 (Fig 5)",
                          "Table 5 (Fig 6)",
                          "Table 6 (Fig 7)",
                          "Table 7 (Fig S1)",
                          "Table 8 (Fig S2)",
                          "Table 9 (Fig S3)")
  

#saveRDS(all_tables, file = "output/all_tables.RDS")



