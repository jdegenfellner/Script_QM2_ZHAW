#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse)
data(NHANES)
head(NHANES)

df <- NHANES # shorter
df_age <- df %>% dplyr::filter(Age >= 20)

df_age %>%
  ggplot(aes(x = BPSysAve)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, fill = "lightblue", alpha = 0.6) +  
  geom_density(color = "blue", linewidth = 1) +  
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df_age$BPSysAve, na.rm = TRUE), 
                sd = sd(df_age$BPSysAve, na.rm = TRUE)), 
    color = "red", linewidth = 1, linetype = "dashed"
  ) +  # Theoretical normal curve
  labs(
    x = "Systolic Blood Pressure (BPSysAve)", 
    y = "Density", 
    title = "Distribution of Systolic Blood Pressure with Normal Curve"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))



# Priors:
# manipulate coefficients to see how they affect the outcome
set.seed(123)
n_sims <- dim(df_age)[1] # 7235 participants
beta_0_vec <- rnorm(n_sims, 120, 2)
beta_1_vec <- rnorm(n_sims, -0.3, 5)
beta_2_vec <- rnorm(n_sims, 0.3, 1)
beta_3_vec <- rnorm(n_sims, 0, 5)
sigma_vec <- runif(n_sims, 0, 10) # controls the spread of the outcome
age_vec <- sample(df_age$Age - mean(df_age$Age), n_sims, replace = TRUE) # centered
phys_act <- as.numeric(sample(df_age$PhysActive, n_sims, replace = TRUE)) - 1
gender_vec <- as.numeric(sample(df_age$Gender, n_sims, replace = TRUE)) - 1
BPSysAve_sim <- rnorm(n_sims, beta_0_vec + 
                        beta_1_vec*phys_act + 
                        beta_2_vec*age_vec + 
                        beta_3_vec*gender_vec, sigma_vec)
hist(BPSysAve_sim, breaks = 50, main = "Prior predictive checks for BPSysAve")
summary(BPSysAve_sim) # -> min too low

summary(df$BPSysAve) # not too bad with respect to range.
# maybe one could try a truncated normal distribution as well for the outcome.
