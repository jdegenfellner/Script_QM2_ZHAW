library(rethinking)
library(tidyverse)
data(Howell1)
d <- Howell1

d$weight_s <- scale(d$weight)
# Fit the model
m4.2 <- lm(height ~ weight_s + I(weight_s^2), data = d)
summary(m4.2)
# (Intercept)   146.6604

mean(d$height) # 138.2636

# The reason for a difference is:

# The intercept is the (model-based) average height of a person with
# average weight. The overall mean has no such restriction.
# The sample mean weights each observation equally,
# while the height of a person with average weight is
# determined model-based. One could also have other models and 
# obtain different intercepts.
