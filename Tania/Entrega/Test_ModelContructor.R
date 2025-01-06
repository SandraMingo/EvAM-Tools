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
Build.Q(Theta)

# Build.Q.Extended fuction:
Build.Q.Extended(Theta, Omega)


# Q.Diag fuction:
Q.Diag(Theta)

# Q.Build fuction:

#Simulation-------------------------

set.seed(1)

#Create a true MHN with random parameters (in log-space)
result  <- Random.Theta.Omega(n=8, sparsity=0.50)
pTh <- Generate.pTh(Theta.true)

## #Estimate the model from an empirical sample
## pD  <- Finite.Sample(pTh, 500)
## Theta.hat <- Learn.MHN(pD, lambda=1/500)
## KL.Div(pTh, Generate.pTh(Theta.hat))

## #Given the true distribution, parameters can often be recovered exactly
## Theta.rec <- Learn.MHN(pTh, lambda=0, reltol=1e-13)

Finite.Sample <- function(pTh, k){
  N <- length(pTh)
  tabulate(sample(1:N, k, prob=pTh, replace=T), nbins=N) / k
}

pD  <- Finite.Sample(pTh, 500)
Learn.Indep.Omega (pD, Omega)

