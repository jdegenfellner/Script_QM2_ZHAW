# Define the covariance matrix
sigma <- matrix(c(0.75, 0.5, 0.5, 0.75), ncol = 2)

# Extract variances (diagonal elements)
var_x <- sigma[1, 1]
var_y <- sigma[2, 2]

# Extract covariance (off-diagonal element)
cov_xy <- sigma[1, 2]  # or sigma[2,1] (both are the same)

# Compute standard deviations
sd_x <- sqrt(var_x)
sd_y <- sqrt(var_y)

# Compute correlation
cor_xy <- cov_xy / (sd_x * sd_y)

# Construct correlation matrix manually
cor_matrix <- matrix(c(1, cor_xy, cor_xy, 1), ncol = 2)

# Print results
print("Variance of X:")
print(var_x)
print("Variance of Y:")
print(var_y)
print("Covariance between X and Y:")
print(cov_xy)
print("Standard Deviation of X:")
print(sd_x)
print("Standard Deviation of Y:")
print(sd_y)
print("Correlation Matrix:")
print(cor_matrix)

# Verify with built-in function
print("Correlation Matrix using cov2cor():")
print(cov2cor(sigma))  # Should match
