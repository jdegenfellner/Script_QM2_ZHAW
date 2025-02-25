#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse, ggdag, dagitty)
data(NHANES)
head(NHANES)
unique(NHANES$SurveyYr)

colnames(NHANES)


df <- NHANES # shorter

# Define the DAG
dag <- dagitty('dag {
  PhysActive -> BPSysAve
  Age -> PhysActive
  Age -> BPSysAve
  PhysActive -> BMI
  BMI -> BPSysAve
  Gender -> PhysActive
  Gender -> BMI
  Gender -> BPSysAve
}')

# Set node coordinates for a nice layout
dagitty::coordinates(dag) <- list(
  x = c(PhysActive = 0, BPSysAve = 2, Age = 1, BMI = 1, Gender = 0.5),
  y = c(PhysActive = 1, BPSysAve = 1, Age = 2, BMI = 1.5, Gender = 2)
)

# Plot the DAG with larger node labels and bubbles
ggdag(dag) + 
  theme_minimal() + 
  geom_dag_point(size = 20, color = "black") +  # Increase node size
  geom_dag_text(size = 2.5, color = "white") +    # Increase label size
  ggtitle("Hypothesized Relationships") + 
  theme(plot.title = element_text(hjust = 0.5))

# explore associations-------
# Age and BMI
df %>% 
  dplyr::filter(Age >= 20) %>%
  dplyr::select(Age, BMI) %>%
  ggplot(aes(Age, BMI)) +
  geom_point() + 
  geom_smooth(method = "loess", se = TRUE)
# once could argue that there is a weakly curved relationship between Age and BMI
summary(lm(BMI ~ Age + I(Age^2), data = df_age)) 
# which can be confirmed due to the very large sample size

# Gender and PyhsActive
df_age <- df %>% 
  dplyr::filter(Age >= 20)
table(df_age$Gender, df_age$PhysActive)
# add margins to table:
addmargins(table(df_age$Gender, df_age$PhysActive))/sum(table(df_age$Gender, df_age$PhysActive))
chisq.test(table(df_age$Gender, df_age$PhysActive))

df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x=Gender, fill=PhysActive)) + 
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", title = "Physical Activity by Gender") +
  theme_minimal()

# gender and PBSysAve
hist(df$BPSysAve)
t.test(df$BPSysAve ~ df$Gender)
df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = as.factor(Gender), y = BPSysAve)) +  # Corrected `y`
  geom_boxplot() +
  labs(x = "Gender", y = "Systolic Blood Pressure (BPSysAve)", title = "Blood Pressure by Gender") +
  theme_minimal()

# age and BPSysAve
df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = Age, y = BPSysAve)) +  # Corrected `y`
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)
  labs(x = "Age", y = "Systolic Blood Pressure (BPSysAve)", title = "Blood Pressure by Age") +
  theme_minimal()
  
# age and physical activity
df %>%
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = Age, fill = PhysActive)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age", y = "Density", title = "Physical Activity by Age") +
  theme_minimal()
t.test(df_age$Age ~ df_age$PhysActive)



# Research question: Influence of Physical Activity on Blood Pressure
# Estimate the total effect of Physical Activity on Blood Pressure
# using a linear regression model
lm(BPSysAve ~ PhysActive, data = df_age) %>% 
  summary()

impliedConditionalIndependencies(dag)
adjustmentSets(dag, exposure = "PhysActive", outcome = "BPSysAve")

# Testing implied conditional independencies:
# Age _||_ BMI | Gnder, PhyA
summary(lm(Age ~ BMI + Gender + PhysActive, data = df %>% dplyr::filter(Age >= 20)))
# check

# Age _||_ Gndr
summary(lm(Age ~ Gender, data = df %>% dplyr::filter(Age >= 20)))
# check by logic

# outcome
hist(df_age$BPSysAve)
#shapiro.test(df_age[sample(1:nrow(df_age),4000,replace = FALSE),]$BPSysAve)
df_age %>%
  ggplot(aes(x = BPSysAve)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", alpha = 0.6) +  # Ensure density scaling
  geom_density(color = "blue", size = 1) +  # Smoothed density of actual data
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df_age$BPSysAve, na.rm = TRUE), sd = sd(df_age$BPSysAve, na.rm = TRUE)), 
    color = "red", size = 1, linetype = "dashed"
  ) +  # Theoretical normal curve
  labs(
    x = "Systolic Blood Pressure (BPSysAve)", 
    y = "Density", 
    title = "Distribution of Systolic Blood Pressure with Normal Curve"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

summary(df_age$BPSysAve)

















# new DAG

# Define the DAG
dag <- dagitty('dag {
  PhysActive -> BPSysAve
  Age -> PhysActive
  Age -> BPSysAve
  Age -> BMI
  BMI -> PhysActive
  BMI -> BPSysAve
  Gender -> PhysActive
  Gender -> BMI
  Gender -> BPSysAve
}')

# Set node coordinates for a nice layout
dagitty::coordinates(dag) <- list(
  x = c(PhysActive = 0, BPSysAve = 2, Age = 1, BMI = 1, Gender = 0.5),
  y = c(PhysActive = 1, BPSysAve = 1, Age = 2, BMI = 1.5, Gender = 2)
)

# Plot the DAG with larger node labels and bubbles
ggdag(dag) + 
  theme_minimal() + 
  geom_dag_point(size = 20, color = "black") +  # Increase node size
  geom_dag_text(size = 2.5, color = "white") +    # Increase label size
  ggtitle("Hypothesized Relationships") + 
  theme(plot.title = element_text(hjust = 0.5))

impliedConditionalIndependencies(dag)
adjustmentSets(dag, exposure = "PhysActive", outcome = "BPSysAve")



set.seed(123)
n_sims <- 10^4
beta_0_vec <- rnorm(n_sims, 140, 20)
beta_1_vec <- rnorm(n_sims, 0, 50)
beta_2_vec <- rnorm(n_sims, 0, 10)
sigma_vec <- runif(n_sims, 0, 50)
BPSysAve_sim <- rnorm(n_sims, beta_0_vec + beta_1_vec + beta_2_vec, sigma_vec)
length(BPSysAve_sim) # 10^4
