library(rethinking)
data("Howell1")
d <- Howell1
str(d)

plot(d$weight, d$height)

# Define the SSE function
sse_function <- function(params, data) {
  a <- params[1]
  b1 <- params[2]
  b2 <- params[3]
  
  # Predicted heights based on the exponential model
  predicted <- a + b1 * exp(b2 * data$weight)
  
  # Compute SSE
  sse <- sum((data$height - predicted)^2)
  return(sse)
}

# Set better initial guesses
a_start <- 120  # Intercept around average height
b1_start <- -50  # Negative to avoid unrealistic growth
b2_start <- -0.05  # Negative to ensure decay

initial_params <- c(a = a_start, b1 = b1_start, b2 = b2_start)

# Constrain b1 and b2 to be negative
optim_results <- optim(
  par = initial_params,
  fn = sse_function,
  data = d,
  method = "L-BFGS-B",
  lower = c(-Inf, -200, -1),   # b1 and b2 must be negative
  upper = c(Inf, -1, -0.001)  # b1 and b2 stay negative but not extreme
)

# Extract optimized parameters
best_params <- optim_results$par
print(best_params)

# Generate predictions using optimized parameters
d$predicted_height <- best_params[1] + best_params[2] * exp(best_params[3] * d$weight)

# Plot actual vs. predicted
plot(d$weight, d$height, col = "gray", pch = 16, xlab = "Weight", ylab = "Height",
     main = "Constrained Exponential Model Fit")

# Add corrected fitted curve
lines(sort(d$weight), sort(d$predicted_height), col = "red", lwd = 2)
