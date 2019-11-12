

#### plotting
library(tidyverse)
sim <- read_csv("../output/sims_summary_gathered.csv", guess_max = 100000)

sim <- sim %>% 
  mutate(model = as.factor(model),
         alpha_U = as.factor(alpha_U),
         beta_U = as.factor(beta_U),
         beta_A = as.factor(beta_A),
         sigma_Y = as.factor(sigma_Y),
         parameter = as.factor(parameter)
         ) %>% 
  rename(Model = model)
levels(sim$parameter) <- c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]", "MSE")
levels(sim$Model) <- c("CompCase", "1: MultImp", "1: RegImp", "1: MeanImp", "2: MultImp", "2: RegImp", "2: MeanImp", "3: MultImp", "3: RegImp", "3: MeanImp")

sim <- sim %>% filter(Model != "3: MeanImp") # this interaction model is never defined because A is constant when R=0.

cols <- c("black", "light green", "green", "green4", "light blue", "blue", "royalblue4", "pink", "violetred")
parameter_labs <- c(expression(gamma[A]), expression(gamma[R]), expression(gamma[RA]), "MSE")
names(parameter_labs) <- c("gamma_A", "gamma_R", "gamma_RA", "mse")


# 1st graph: scen (i) and (ii)
sim_i_ii <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, sigma_A == 1, gamma_U == 1, !rA_equals_U) 

levels(sim_i_ii$sigma_Y) <- c("sigma[Y] == 0.1", "sigma[Y] == 0.5", "sigma[Y] == 1")


vline_aes <- crossing(
  sigma_Y = levels(sim_i_ii$sigma_Y),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
 vline_aes$loc <- c(1.25, 1, 0.5, 1)
 vline_aes$type <- c("c", "a", "a", "b")
 vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_i_ii <- sim_i_ii %>% 
  ggplot(aes(x = beta_U, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(sigma_Y) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(beta[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


#2nd graph: scen (ii) and (iii) under rA_equals_U
sim_ii_iii_eq <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_U == 0, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, sigma_A == 1, gamma_U == 1, rA_equals_U)

levels(sim_ii_iii_eq$sigma_Y) <- c("sigma[Y] == 0.1", "sigma[Y] == 0.5", "sigma[Y] == 1")


vline_aes <- crossing(
  sigma_Y = levels(sim_ii_iii_eq$sigma_Y),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 1, 0.5, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_ii_iii_eq <- sim_ii_iii_eq %>% 
  ggplot(aes(x = alpha_U, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(sigma_Y) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(alpha[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()

#3rd graph: scenario (iii) - correcting for unmeasured confounding?
sim_iii <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U)

levels(sim_iii$beta_U) <- c("beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_iii$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 1, 0.5, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_iii <- sim_iii %>% 
  ggplot(aes(x = alpha_U, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(alpha[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()

#4th graph: scenario (iv) - complete case should be unbiased
sim_iv <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, !rA_equals_U)

levels(sim_iv$sigma_Y) <- c("sigma[Y] == 0.1", "sigma[Y] == 0.5", "sigma[Y] == 1")

vline_aes <- crossing(
  sigma_Y = levels(sim_iv$sigma_Y),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 1, 0.5, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_iv <- sim_iv %>% 
  ggplot(aes(x = beta_A, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(sigma_Y) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


#5th graph: scenario (v)
sim_v <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U)

levels(sim_v$beta_U) <- c("beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_v$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 1, 0.5, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_v <- sim_v %>% 
  ggplot(aes(x = beta_A, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


#6th graph: scenario (vi)
sim_vi <- sim %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U)

levels(sim_vi$beta_U) <- c("beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_vi$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 1, 0.5, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_vi <- sim_vi %>% 
  ggplot(aes(x = beta_A, y = med, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()



