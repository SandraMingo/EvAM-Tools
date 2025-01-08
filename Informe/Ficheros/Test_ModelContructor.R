#######################################
############TEST#######################
#######################################

# Random.Theta.Omega fuction:
Random.Theta.Omega(3)
result <- Random.Theta.Omega(4, sparsity = 0.5)
Theta <- result$Theta
Omega <- result$Omega

#######################################################
######## Test para Random.Theta.Omega ##################
#######################################################

test_random_theta_omega <- function() {
  # Probar con un tamaño pequeño de n
  n <- 5
  result <- Random.Theta.Omega(n)
  
  # Verificar dimensiones de la salida
  stopifnot(ncol(result$Theta) == n && nrow(result$Theta) == n)
  stopifnot(length(result$Omega) == n)
  
  # Verificar que la diagonal de Theta no tiene NA
  stopifnot(all(!is.na(diag(result$Theta))))
  
  # Verificar que Omega es un vector de longitud n
  stopifnot(length(result$Omega) == n)
  
  print("Test Random.Theta.Omega pasó con éxito.")
}
test_random_theta_omega()

#########################################################
#########################################################

# Q.Subdiag fuction:
Q.Subdiag(Theta, i = 2)

# Build.Q fuction:
Q <- Build.Q(Theta)
all.equal(colSums(as.matrix(Q)), rep(0, ncol(Q)))

# Build.Q.Extended fuction:
Q_extended <-Build.Q.Extended(Theta, Omega)
all.equal(colSums(as.matrix(Q_extended)), rep(0, ncol(Q_extended)))

