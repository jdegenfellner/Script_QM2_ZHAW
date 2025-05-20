library(ggdag)
library(dagitty)

# Define the DAG
dag <- dagitty("dag {
  Smoking -> LungCancer
  Smoking -> Lighter
  Lighter -> LungCancer
}")

# Plot the DAG
ggdag(dag, layout = "circle") +
  geom_dag_point(size = 20, color = "black") +
  geom_dag_text(size = 2.5, color = "white") +
  coord_cartesian(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
  theme_void()

# Set seed for reproducibility
set.seed(123)

# Number of individuals
n <- 10000

# Simulate Smoking (1 = Smoker, 0 = Non-Smoker)
smoking <- rbinom(n, 1, 0.25)  # 25% smokers

# Simulate Carrying a Lighter (dependent on smoking)
lighter <- ifelse(smoking == 1, 
                  rbinom(n, 1, 0.95), 
                  rbinom(n, 1, 0.05))  # 95% of smokers, 5% of non-smokers

# Simulate Lung Cancer (dependent on smoking)
lung_cancer <- ifelse(smoking == 1, 
                      rbinom(n, 1, 0.20), 
                      rbinom(n, 1, 0.02))  # 20% of smokers, 2% of non-smokers

# Store in dataframe
df <- data.frame(smoking, lighter, lung_cancer)

# Check the data
table(df$smoking, df$lung_cancer)  # Cross-tab for lung cancer rates
table(df$lighter, df$lung_cancer)  # Cross-tab for lighters

# chisquare test for independence
chisq.test(table(df$smoking, df$lung_cancer)) # dependent
chisq.test(table(df$lighter, df$lung_cancer)) # dependent

# Logistic regression for lighter as only predictor
model_ligher_only <- glm(lung_cancer ~ lighter, 
             data = df, family = "binomial")
summary(model_ligher_only) # beta for lighter not zero.
confint(model_ligher_only) # lighter      2.076103  2.458932
exp(coef(model_ligher_only)) # odds for lung_canter 9.62-times higher for carriers of lighters

# Logistic regression adjusting the relationship lighter->lung_cancer by smoking.
model <- glm(lung_cancer ~ smoking + lighter, 
             data = df, family = "binomial")
summary(model) # beta for lighter is now 0
confint(model) # lighter     -0.3789814  0.4126977
# -> spurious relationship disappears as one would expect.
