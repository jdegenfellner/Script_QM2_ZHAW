# ex 9

library(rethinking)
library(conflicted)
library(car)
conflicts_prefer(stats::sd)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

mod <- lm( height ~ weight , data = d2 )
round(mean(residuals(mod)),3) # 0, check!
sd(residuals(mod)) # 5.079086

res_sim <- rnorm(25, mean = 0, sd = 5.079086)
qqPlot(res_sim) 
# -> repeat the above 2 lines many times to strengthen your 
#    intuition about the distribution of residuals
# Always remember: in this case, we KNOW the residuals are normal

t_sim <- rt(352, df = 100)
qqPlot(t_sim) # check counterfactual
