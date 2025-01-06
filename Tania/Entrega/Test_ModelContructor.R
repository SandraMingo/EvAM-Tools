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

# Test para Q.Subdiag
test_q_subdiag <- function() {
  Theta <- matrix(c(0.5, 0.2, 0.3, 0.7, 0.4, 0.6), nrow = 3, ncol = 3)
  result <- Q.Subdiag(Theta, 2)
  
  # Verificar que la longitud del resultado sea la correcta
  stopifnot(length(result) == 3)
  
  # Verificar que la tasa base s esté correcta
  stopifnot(result[1] == exp(Theta[2, 2]))
  
  print("Test Q.Subdiag pasó con éxito.")
}
test_q_subdiag()


# Build.Q fuction:
Build.Q(Theta)

# Build.Q.Extended fuction:
Build.Q.Extended(Theta, Omega)


# Q.Diag fuction:
Q.Diag(Theta)

# Q.Build fuction:
Learn.Indep.Omega (Omega=Omega)

