library(pacman)
p_load(tidyverse, readxl, ggExtra)

# Read file
url <- "https://raw.githubusercontent.com/jdegenfellner/Script_QM2_ZHAW/main/data/chapter%205_assignment%201_2_wide.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, temp_file, mode = "wb")  # mode="wb" is important for binary files
df <- read_excel(temp_file)

head(df)
dim(df)

# As in the book, let's randomly select 50 patients.
set.seed(123)
df <- df %>% sample_n(50)
dim(df)

# "as" = affected shoulder
# "nas" = not affected shoulder

# nas-------

df <- df %>%
  mutate(diff = abs(ROMnas.Peter - ROMnas.Mary)) %>%  # Compute absolute difference
  mutate(ID = row_number())  # Add an ID column
  
max_diff_point <- df %>%
  dplyr::filter(diff == max(diff, na.rm = TRUE))  # Find the row with the max difference

df %>%
ggplot(aes(x = ROMnas.Peter, y = ROMnas.Mary)) +
  geom_point() + 
  geom_point(data = max_diff_point, aes(x = ROMnas.Peter, y = ROMnas.Mary), 
             color = "blue", size = 4) +  # Highlight max difference point
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  ggtitle("ROMnas.Peter vs. ROMnas.Mary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = max_diff_point$ROMnas.Peter, 
           y = max_diff_point$ROMnas.Mary, 
           label = paste0("Max Diff: ", round(max_diff_point$diff, 2)), 
           vjust = -1, color = "blue", size = 4)

# average abs. difference:
mean(df$diff, na.rm = TRUE) # 7.2
cor(df$ROMnas.Peter, df$ROMnas.Mary, use = "complete.obs") 

# mean difference
mean(df$ROMnas.Peter - df$ROMnas.Mary, na.rm = TRUE) 



# as---------
df <- df %>%
  mutate(diff = abs(ROMas.Peter - ROMas.Mary))  # Compute absolute difference

max_diff_point <- df %>%
  dplyr::filter(diff == max(diff, na.rm = TRUE))  # Find the row with the max difference
dim(df)
p <- df %>%
  ggplot(aes(x = ROMas.Peter, y = ROMas.Mary)) +
  geom_point() + 
  geom_point(data = max_diff_point, aes(x = ROMas.Peter, y = ROMas.Mary), 
             color = "blue", size = 4) +  # Highlight max difference point
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  ggtitle("ROMas.Peter vs. ROMas.Mary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = max_diff_point$ROMas.Peter, 
           y = max_diff_point$ROMas.Mary, 
           label = paste0("Max Diff: ", round(max_diff_point$diff, 2)), 
           vjust = -1, color = "blue", size = 4)

# average abs. difference:
mean(df$diff, na.rm = TRUE) # 7.78
cor(df$ROMas.Peter, df$ROMas.Mary, use = "complete.obs") # 0.8284572

hist(df$ROMas.Peter)
hist(df$ROMas.Mary)

# mean difference
mean(df$ROMas.Peter - df$ROMas.Mary, na.rm = TRUE) 

ggMarginal(p, type = "density", fill = "gray", color = "black")



# ICC (consistency) using the irr package:---------
library(irr)
irr::icc(as.matrix(df[, c("ROMas.Peter", "ROMas.Mary")]), 
    model = "oneway", type = "consistency")
# 0.851


# _Verify with lmer:--------

data_ <- df %>% 
  mutate(ID = row_number()) %>%
  dplyr::select(ID,ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))

library(lme4)
m5.2 <- lmer(ROM ~ (1 | ID), data = data_)
summary(m5.2)
print(VarCorr(m5.2), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 270.99  
# Residual              47.35 

# ICC = 
270.99 / (270.99 + 47.35) # 
# 0.8512597
# perfect match.



# _Verify using rethinking---------
library(rethinking)
library(tictoc)

data_$ID

tic()
m5.1 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma),
    
    # Patient-specific intercepts (random effects)
    mu <- a[ID],  
    a[ID] ~ dnorm(a_bar, sigma_ID),  # Hierarchical structure for patients
    
    # Priors for hyperparameters
    a_bar ~ dnorm(66, 20),  # Population-level mean
    sigma_ID ~ dunif(0,20),  # Between-patient standard deviation
    sigma ~ dunif(0,20)  # Residual standard deviation
  ), 
  data = data_, 
  chains = 4, cores = 4
)
toc() # 7s

precis(m5.1, depth = 2)

post <- extract.samples(m5.1)
var_patients <- mean(post$sigma_ID^2)  # Between-patient variance
var_residual <- mean(post$sigma^2)     # Residual variance
var_patients / (var_patients + var_residual) # ICC
# 0.846323 (vs 0.851)
# not too bad



# ICC agreement with bias 5 degrees Mary-Peter ~ 5:

#introduce bias:
# mean diff before in df:
mean(df$ROMas.Mary - df$ROMas.Peter, na.rm = TRUE) # - 1.22
# hence should be -1.22 + 5 = 3.78 afterwards
data_ <- data_ %>%
  mutate(ROM = ROM + ifelse(Rater == "ROMas.Mary", 5, 0))

# mean Mary
mean(data_$ROM[data_$Rater == "ROMas.Mary"]) # 69.98
# mean Peter
mean(data_$ROM[data_$Rater == "ROMas.Peter"], na.rm = TRUE) # 66.2
# diff
mean(data_$ROM[data_$Rater == "ROMas.Mary"]) - 
  mean(data_$ROM[data_$Rater == "ROMas.Peter"], na.rm = TRUE) # 3.78
# 3.78

library(rethinking)

m5.2 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma_eps),
    
    # Model for mean ROM with patient and rater effects
    mu <- alpha[ID] + beta[Rater],  
    
    # Patient-specific random effects
    alpha[ID] ~ dnorm(alpha_mean, sigma_alpha),  
    
    # Rater effect (Peter/Mary)
    beta[Rater] ~ dnorm(0, sigma_beta),  
    
    # Priors for hyperparameters
    alpha_mean ~ dnorm(66, 20),  # Population mean ROM
    sigma_alpha ~ dunif(0, 30),  # Between-patient SD
    sigma_beta ~ dunif(0, 10),   # Rater SD
    sigma_eps ~ dunif(0, 40)     # Residual SD
  ), 
  data = data_, 
  chains = 8, cores = 4
)

precis(m5.2, depth = 2)

# check systematic difference for rater in posterior
post <- extract.samples(m5.2)
mean(post$beta[,1] - post$beta[,2])  # 3.701551 # makes sense with the above

# ICC agreement:
post <- extract.samples(m5.2)
(var_patients <- mean(post$sigma_alpha^2))  # Between-patient variance
(var_raters <- mean(post$sigma_beta^2))     # Rater variance
(var_residual <- mean(post$sigma_eps^2))    # Residual variance

# ICC_agreement = 
var_patients / (var_patients + var_raters + var_residual)
# 0.7250569/0.7798

str(data_)
# tibble [100 × 3] (S3: tbl_df/tbl/data.frame)
# $ ID   : int [1:100] 1 1 2 2 3 3 4 4 5 5 ...
# $ Rater: Factor w/ 2 levels "ROMas.Mary","ROMas.Peter": 2 1 2 1 2 1 2 1 2 1 ...
# $ ROM  : num [1:100] 66 75 65 68 96 87 75 85 62 59 ...

# verify with irr:
# use df and introduce bias there too 
head(df)
df <- df %>%
  mutate(ROMnas.Mary = ROMnas.Mary + 5)

mean(df$ROMnas.Mary) # 84.04
mean(df$ROMnas.Peter) #  78.48
# diff
mean(df$ROMnas.Mary) - mean(df$ROMnas.Peter) # 5.56

irr::icc(as.matrix(df[, c("ROMas.Peter", "ROMas.Mary")]), 
    model = "twoway", type = "agreement")


# try lmer:
# mean Mary
mean(data_$ROM[data_$Rater == "ROMas.Mary"]) # 69.98
# mean Peter
mean(data_$ROM[data_$Rater == "ROMas.Peter"], na.rm = TRUE) # 66.2

m5.3 <- lmer(ROM ~ (1 | ID) + (1 | Rater), data = data_)
summary(m5.3)
print(VarCorr(m5.3), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 270.882 
# Rater    (Intercept)   6.193 
# Residual              47.557 


# ICC =
270.882 / (270.882 + 6.193 + 47.557) # 
# 0.8344279
