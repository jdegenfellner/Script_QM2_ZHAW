# create data from the regression model with changed parameter values

library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,] # only adults
summary(d2$weight)
sd(d2$weight)
dim(d2)

height <- numeric(352)
weight <- numeric(352)

for (i in 1:352) {
  weight[i] <- rnorm(1, 45, 6.45) # draw new weights
  height[i] <- 113.87939 + 0.90574 * weight[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}

# manual estimates:
# beta_1 = 
( beta_1_hat <- cor(weight, height)*sd(height)/(sd(weight)) )
# beta_0 =
( beta_0_hat <- mean(height) - beta_1_hat * mean(weight) )
# -> reconstructs nicely the true coefficients!

# plot height weight
plot(height, weight, xlab = "height", ylab = "weight", main = "height weight plot")
