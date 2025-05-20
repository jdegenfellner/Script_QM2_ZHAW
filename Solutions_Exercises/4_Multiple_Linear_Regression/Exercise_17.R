#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse, data.table)
data(NHANES)
head(NHANES)

df <- NHANES # shorter
df <- df %>% dplyr::filter(Age >= 20) %>%
  dplyr::select(BPSysAve, PhysActive, Age, Gender) %>%
  drop_na(BPSysAve)
df <- as.data.table(df) # needed for subsetting below
dim(df) # 6971
sum(is.na(df)) # 0

Age_mean <- mean(df[Age >= 20,]$Age, na.rm = TRUE) # adults

#set.seed(122)
m_NHANES_lnorm <- quap( # quap works as long as we do not have multilevel models
  alist(
    BPSysAve ~ dlnorm(lmu, lsd), 
    lsd <- exp(beta_4 + beta_5 * lmu),
    lmu <- beta_0 + beta_1[PhysActive] + beta_2 * (Age - Age_mean) + beta_3[Gender],
    beta_0 ~ dnorm(140, 10),  # 
    beta_1[PhysActive] ~ dnorm(0, 10),  # 
    beta_2 ~ dnorm(0, 10),
    beta_3[Gender] ~ dnorm(0, 10),  # 
    beta_4 ~ dnorm(0, 10),  # 
    beta_5 ~ dnorm(0, 10)  #
  ),
  data = df,
  start = list(beta_0 = 140, beta_1 = c(0.5, 0), beta_2 = 0, beta_3 = c(0.5, 0), beta_4 = 1, beta_5 = 0)
)

precis(m_NHANES_lnorm, depth = 2)

# Make predictions using the improved model:-------
mu_pred <- link(m_NHANES_lnorm)$lmu %>% colMeans()
bp_pred <- exp(mu_pred)  # da lmu auf Log-Skala

# Compare predictions with observed values-------
df$mu_pred <- bp_pred  # fügen wir direkt an den Datensatz an

ggplot(df, aes(x = bp_pred, y = BPSysAve)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Observed vs. Predicted Systolic BP",
    x = "Predicted (model)",
    y = "Observed (NHANES)"
  ) +
  theme_minimal()
