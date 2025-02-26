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



# as


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



# calculate the ICC using the irr package:
library(irr)
irr::icc(as.matrix(df[, c("ROMas.Peter", "ROMas.Mary")]), 
    model = "oneway", type = "consistency")
# 0.851


# verify with lmer:--------
library(lme4)
m5.2 <- lmer(ROM ~ (1 | ID), data = data_)
summary(m5.2)
print(VarCorr(m5.2), comp = "Variance")

# ICC = 
270.99 / (270.99 + 47.35) # 
# 0.8512597




# verify using rethinking---------
library(rethinking)
library(tictoc)

data_ <- df %>% dplyr::select(ID,ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))


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
# 0.846323
# not too bad

