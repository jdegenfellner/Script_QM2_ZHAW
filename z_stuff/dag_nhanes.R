#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse)
data(NHANES)
head(NHANES)
unique(NHANES$SurveyYr)

colnames(NHANES)


df <- NHANES # shorter

library(ggdag)
library(dagitty)

# Define the DAG
dag <- dagitty('dag {
  PhysActive -> BPSysAve
  Age -> PhysActive
  Age -> BPSysAve
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
  ggtitle("Causal DAG: Predictors of Blood Pressure (BPSysAve)") + 
  theme(plot.title = element_text(hjust = 0.5))

# explore associations-------
# Age and BMI
df %>% 
  dplyr::filter(Age >= 20) %>%
  dplyr::select(Age, BMI) %>%
  ggplot(aes(Age, BMI)) +
  geom_point()
# not much correlation...  

# Gender and PyhsActive
table(df$Gender, df$PhysActive)
# add margins to table:
addmargins(table(df$Gender, df$PhysActive))/sum(table(df$Gender, df$PhysActive))
chisq.test(table(df$Gender, df$PhysActive))

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
