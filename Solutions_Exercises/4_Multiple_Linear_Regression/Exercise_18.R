#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse)
data(NHANES)
head(NHANES)

df <- NHANES # shorter
df <- df %>% dplyr::filter(Age >= 20) %>%
  dplyr::select(BPSysAve, PhysActive, Age, Gender) %>%
  drop_na() %>%
  sample_n(100) # take only 100 obs.
dim(df) # 100
sum(is.na(df)) # 0



# Fit model on smaller data set----------

set.seed(122)
m_NHANES_lnorm <- quap(
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

# difference in levels of PhysActive and Gender (catorical variables):
post_lnorm <- extract.samples(m_NHANES_lnorm)
post_lnorm$diff_PhysActive <- post_lnorm$beta_1[,2] - post_lnorm$beta_1[,1]
post_lnorm$diff_G <- post_lnorm$beta_3[,2] - post_lnorm$beta_3[,1]
#precis(post_lnorm, depth = 2)
post_lnorm_summary <- as.data.frame(precis(post_lnorm, depth = 2))  # Convert to dataframe
post_lnorm_summary[, -which(names(post_lnorm_summary) == "histogram")]  # Remove histogram column

# mean           sd          5.5%         94.5%
#   beta_0           71.841939570 7.1079382496  60.308982344  83.199645434
# beta_2            0.002944244 0.0006329387   0.001926700   0.003957665
# beta_4           -7.910108023 4.6697510241 -15.380407364  -0.511848559
# beta_5            1.211577559 0.9758378823  -0.335435618   2.769298320
# beta_1[1]       -33.673937816 6.1747759540 -43.271867436 -23.563676443
# beta_1[2]       -33.714259979 6.1752345985 -43.308281429 -23.583132294
# beta_3[1]       -33.389383722 6.1394423350 -43.223420405 -23.611574622
# beta_3[2]       -33.350551664 6.1393618162 -43.182885742 -23.581368678
# diff_PhysActive  -0.040322163 0.0271211747  -0.083447603   0.003606194
# diff_G            0.038832058 0.0256356256  -0.002455136   0.079809763
# 

# -> Compared to the results in the full data set, we have a different estimate of 
# beta_4 (with very similar credible interval), beta_5 (wider credible interval) 



# what about the posterior predictive check-----------
# posterior predictive checks
sample_BP <- sim(m_NHANES_lnorm, n = 1000)

# Convert the first 100 rows of the posterior samples into a long format for ggplot
df_posterior <- as.data.frame(t(sample_BP[1:100,])) %>%
  pivot_longer(cols = everything(), names_to = "Simulation", values_to = "BPSysAve_sim")

# Create the plot
ggplot() +
  geom_density(data = df_posterior, aes(x = BPSysAve_sim, group = Simulation), 
               color = "lightblue", alpha = 0.05) +
  geom_density(data = df, aes(x = BPSysAve), color = "green", linewidth = 1.2) +
  labs(title = "Density Estimation: Original vs. Posterior Samples",
       x = "Systolic Blood Pressure",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# there seems to be more variation in the model prediction
# which makes sense due to the smaller sampole size.
