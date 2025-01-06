## require("Matrix")

#Create a random MHN with (log-transformed) parameters Theta and Omega.
#Sparsity is given as percentage.

Random.Theta.Omega <- function(n, sparsity = 0) {
  Theta <- matrix(0, nrow = n, ncol = n)
  
  diag(Theta) <- rnorm(n)
  nonZeros <- sample(which(lower.tri(Theta) | upper.tri(Theta)), 
                     size = floor((n^2 - n) * (1 - sparsity)))
  Theta[nonZeros] <- rnorm(length(nonZeros))
  
  Omega <- exp(rnorm(n))
  
  return(list(Theta = round(Theta, 2), Omega = round(Omega, 2)))
}
