library(rethinking)
data("Howell1")
d <- Howell1
str(d)
d2 <- d[d$age >= 18, ]
mod <- lm(height ~ 1, data=d2)
sqrt(sum(mod$residuals^2) / (nrow(d2) - 1))
sd(d2$height)
