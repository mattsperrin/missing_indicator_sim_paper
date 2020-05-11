#### includes MAR cases, and new set of figures, for revised paper (R1). ####


#### plotting
library(tidyverse)
sim <- read_csv("../output/sims_summary_gathered.csv", guess_max = 100000)


# allow RA = U case to be plotted along the beta_U axis
sim$beta_U[sim$rA_equals_U] <- 1000

sim <- sim %>% 
  mutate(model = as.factor(model),
         alpha_U = as.factor(alpha_U),
         beta_U = as.factor(beta_U),
         beta_A = as.factor(beta_A),
         sigma_Y = as.factor(sigma_Y),
         parameter = as.factor(parameter)
  ) %>% 
  rename(Model = model) %>% 
  filter(parameter != "mse") %>% 
  gdata::drop.levels()

levels(sim$parameter) <- c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")



sim_MNAR <- sim %>% 
  filter(Model %in% c("1N", "1C", "1S", "2S", "3S")) %>% 
  gdata::drop.levels()

sim_MAR <- sim %>% 
  filter(Model %in% c("3N", "3C", "1M", "2M", "3M")) %>% 
  gdata::drop.levels()


levels(sim_MNAR$Model) <- c("CompleteCase", "CompletedData", "MI", "MIMI", "MIMI + Int")
sim_MNAR$Model <- fct_relevel(sim_MNAR$Model, "CompletedData", "CompleteCase", "MI", "MIMI", "MIMI + Int")

levels(sim_MAR$Model) <- c("MI", "MIMI", "CompleteCase", "MIMI + Int", "CompletedData")
sim_MAR$Model <- fct_relevel(sim_MAR$Model, "CompletedData", "CompleteCase", "MI", "MIMI", "MIMI + Int")

cols <- c("black", "green", "light blue", "blue", "royalblue4")
parameter_labs <- c(expression(gamma[A]), expression(gamma[R]), expression(gamma[RA]))
names(parameter_labs) <- c("gamma_A", "gamma_R", "gamma_RA")



#Fig 2: scenarios (i)-(iii) including RA = U case, no interaction
sim_iii_MNAR <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1)

levels(sim_iii_MNAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1", "R[A] == 1 - U")

vline_aes <- crossing(
  beta_U = levels(sim_iii_MNAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")


ggp_iii_MNAR <- sim_iii_MNAR %>% 
  ggplot(aes(x = alpha_U, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(alpha[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()



#Fig 3: scenarios (i)-(iii) including RA = U case, WITH interaction
sim_iii_MNAR_int <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1)

levels(sim_iii_MNAR_int$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1", "R[A] == 1 - U")

vline_aes <- crossing(
  beta_U = levels(sim_iii_MNAR_int$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 0, 0, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_iii_MNAR_int <- sim_iii_MNAR_int %>% 
  ggplot(aes(x = alpha_U, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(alpha[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


#Fig 4: scenarios (i)-(iii) including RA = U case, MAR, WITHOUT interaction
sim_iii_MAR <- sim_MAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, beta_A == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U)%>% 
  gdata::drop.levels()

sim_iii_MAR$Model <- fct_relevel(sim_iii_MAR$Model, "CompletedData")


levels(sim_iii_MAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_iii_MAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")


ggp_iii_MAR <- sim_iii_MAR %>% 
  ggplot(aes(x = alpha_U, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(alpha[U])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()

#Fig 5: scenarios (iv)-(v), without interaction.
sim_v_MNAR <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_v_MNAR$Model <- fct_relevel(sim_v_MNAR$Model, "CompletedData")

levels(sim_v_MNAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_v_MNAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")


ggp_v_MNAR <- sim_v_MNAR %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


#Fig 6: scenarios (iv)-(v), without interaction, MAR
sim_v_MAR <- sim_MAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_v_MAR$Model <- fct_relevel(sim_v_MAR$Model, "CompletedData")

levels(sim_v_MAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_v_MAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")


ggp_v_MAR <- sim_v_MAR %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()


# Fig 7: scenario (vi), no interaction.
sim_vi_MNAR <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_vi_MNAR$Model <- fct_relevel(sim_vi_MNAR$Model, "CompletedData")

levels(sim_vi_MNAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_vi_MNAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")

ggp_vi_MNAR <- sim_vi_MNAR %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()




# Figure S1: : scenarios (iv)-(v), WITH interaction.
sim_v_MNAR_int <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_v_MNAR_int$Model <- fct_relevel(sim_v_MNAR_int$Model, "CompletedData")

levels(sim_v_MNAR_int$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_v_MNAR_int$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 0, 0, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"


ggp_v_MNAR_int <- sim_v_MNAR_int %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()




# Fig S2: scenario (vi), WITH interaction.
sim_vi_MNAR_int <- sim_MNAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0.5, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_vi_MNAR_int$Model <- fct_relevel(sim_vi_MNAR_int$Model, "CompletedData")

levels(sim_vi_MNAR_int$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_vi_MNAR_int$beta_U),
  parameter = c("hat(gamma)[A]", "temp", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1.25, 0, 0, 1)
vline_aes$type <- c("c", "a", "a", "b")
vline_aes$parameter[vline_aes$parameter == "temp"] <- "hat(gamma)[A]"

ggp_vi_MNAR_int <- sim_vi_MNAR_int %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()

# Fig S3: scenario (vi), no interaction.
sim_vi_MAR <- sim_MAR %>% 
  filter(pi_U == 0.5, p_rA == 0.5, alpha_U == 0.5, beta_UA == 0, gamma_A == 1, gamma_UA == 0, gamma_U == 1, sigma_A == 1, sigma_Y == 1, !rA_equals_U) %>% 
  gdata::drop.levels()

sim_vi_MAR$Model <- fct_relevel(sim_vi_MAR$Model, "CompletedData")

levels(sim_vi_MAR$beta_U) <- c("beta[U] == -1", "beta[U] == 0", "beta[U] == 0.1", "beta[U] == 0.5", "beta[U] == 1")

vline_aes <- crossing(
  beta_U = levels(sim_vi_MAR$beta_U),
  parameter = c("hat(gamma)[A]", "hat(gamma)[R]", "hat(gamma)[RA]")
) 
vline_aes$loc <- c(1, 0, 0)
vline_aes$type <- c("c", "a", "a")

ggp_vi_MAR <- sim_vi_MAR %>% 
  ggplot(aes(x = beta_A, y = mean, group = Model, color = Model)) +
  facet_grid(fct_rev(beta_U) ~ parameter, scales = "free", labeller = label_parsed) +
  geom_point(position = position_dodge2(width = 0.5, reverse = TRUE), size = 2) +
  geom_linerange(aes(ymin = low, ymax = high), position = position_dodge2(width = 0.5, reverse = TRUE), size = 1.2) +
  geom_hline(aes(yintercept = loc, linetype = type), vline_aes) +
  theme_minimal(base_size = 22) + theme(panel.spacing = unit(3, "lines")) +
  labs(y = "Estimate", x = expression(beta[A])) +
  scale_color_manual(values = cols) +
  guides(linetype = FALSE) +
  coord_flip()
