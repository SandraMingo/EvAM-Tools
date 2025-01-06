#######################################
############TEST#######################
#######################################

# Random.Theta.Omega fuction:
Random.Theta.Omega(3)
result <- Random.Theta.Omega(4, sparsity = 0.5)
Theta <- result$Theta
Omega <- result$Omega

# Q.Subdiag fuction:
Q.Subdiag(Theta, i = 2)

# Q.Build fuction: