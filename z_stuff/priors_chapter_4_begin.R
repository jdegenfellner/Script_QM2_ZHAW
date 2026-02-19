set.seed(1)

x  <- d$weight
y  <- d$height
xc <- x - mean(x)

# grid für Kurven
xg  <- seq(min(x), max(x), length.out = 200)
xcg <- xg - mean(x)

n_draws <- 100

# ---- Priors (zentriert!)
alpha <- rnorm(n_draws, 155, 15)
b1    <- rnorm(n_draws, 2.5, 1.0)
b2    <- rnorm(n_draws, -0.03, 0.02)

# ---- Plot: Daten + LOESS (rot)
plot(y ~ x, pch = 16, col = rgb(0,0,0,0.35),
     xlab = "weight", ylab = "height")

lo <- loess(height ~ weight, data = d, span = 0.75)
lines(xg, predict(lo, newdata = data.frame(weight = xg)),
      col = "red", lwd = 3)

# ---- 100 prior mean curves
for(j in 1:n_draws){
  mu <- alpha[j] + b1[j]*xcg + b2[j]*(xcg^2)
  lines(xg, mu, col = rgb(0,0,1,0.15), lwd = 2)
}
