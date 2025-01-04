# This example demonstrate the process of creating a model, 
# generating data, and calculating the stationary distribution.

# Load required libraries
library(Matrix)

## Source the MHN_ModelConstructor.R file (assuming it's in the same directory)
#source("MHN_ModelConstructor.R")

# Set the number of events
n <- 3

# Generate random Theta and Omega
set.seed(123)  # for reproducibility
params <- Random.Theta.Omega(n, sparsity = 0.2)
Theta <- params$Theta
Omega <- params$Omega

cat("Theta matrix:\n")
print(Theta)
cat("\nOmega vector:\n")
print(Omega)

# Build the extended Q matrix
Q_bar <- Build.Q.Extended(Theta, Omega)

cat("\nExtended Q matrix (Q_bar):\n")
print(as.matrix(Q_bar))

# Generate a random initial distribution
p0 <- runif(2^n)
p0 <- p0 / sum(p0)

cat("\nInitial distribution:\n")
print(p0)

# Calculate the stationary distribution
p_B_inf <- Stationary.Distribution(Theta, Omega, p0)

cat("\nStationary distribution:\n")
print(p_B_inf)

# Calculate the log-likelihood for a single observation
# (In practice, you would do this for multiple observations)
observed_state <- sample(1:2^n, 1)  # Randomly select an observed state
log_likelihood <- log(p_B_inf[observed_state])

cat("\nLog-likelihood for observed state", observed_state, ":", log_likelihood, "\n")

