#set.seed(1)

n <- 4

df <- data.frame(
  X1 = rnorm(n),
  X2 = rnorm(n),
  X3 = rnorm(n),
  Y = rnorm(n)
)

df

m <- lm(Y ~ X1 + X2 + X3, data = df)
summary(m)

# sum of residual squares (SSR)
sum(resid(m)^2) # 0

summary(m)$r.squared
summary(m)$sigma
resid(m)
fitted(m) - df$Y
