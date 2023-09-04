# Load the required package
library(MASS)

# Set the means and covariance matrix
p <-  3
means <- rep(0,p)  # Means of the variables
off_diag_value <- 0.5

# Create the covariance matrix
cov_matrix <- diag(p)
cov_matrix[lower.tri(cov_matrix)] <- off_diag_value
cov_matrix[upper.tri(cov_matrix)] <- off_diag_value


# Set the sample size
sample_size <- 1e4

# Generate multivariate normal samples
samples <- mvrnorm(n = sample_size, mu = means, Sigma = cov_matrix)

colMeans(samples)
cov(samples)
