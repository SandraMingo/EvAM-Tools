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


Random.Theta.Omega <- function(n, sparsity = 0) {
  Theta <- matrix(0, nrow = n, ncol = n)
  
  diag(Theta) <- NA
  nonZeros <- sample(which(!is.na(Theta)), size = floor((n^2 - n) * (1 - sparsity)))
  
  Theta[nonZeros] <- rnorm(length(nonZeros))
  diag(Theta) <- rnorm(n)
  
  Omega <- exp(rnorm(n))
  
  # Devolver los resultados redondeados
  return(list(Theta = round(Theta, 2), Omega = round(Omega, 2)))
}

######################################################################################

# Create a single subdiagonal of Q from the ith row in Theta.
# It does not depend from Omega so theres no need to change.
Q.Subdiag <- function(Theta, i){
  row <- Theta[i,]
  n <- length(row)
  
  # Empieza la subdiagonal con la tasa base de Theta_ii. 
  s <- exp(row[i])
  
  # Duplica para cada factor Theta_ij, exceptuando cuando i = j.
  for(j in 1:n){
    s <- c(s, s * exp(row[j]) * (i != j))
  }
  
  return(s)
}

#Con Omega --> No

Q.Subdiag <- function(Theta, Omega, i) {
  row <- Theta[i, ]
  n <- length(row)
  
  # Inicializar la subdiagonal con el término base modificado por Omega
  s <- exp(row[i]) * Omega[i]
  
  # Iterar por la fila para calcular los términos adicionales
  for (j in 1:n) {
    s <- c(s, s * exp(row[j]) * Omega[j] * (i != j))
  }
  
  return(s)
}

#######
## No change

#Build the transition rate matrix Q from its subdiagonals.

Build.Q <- function(Theta){
  n <- nrow(Theta)
  
  Subdiags <- c()
  for(i in 1:n){
    Subdiags <- cbind(Subdiags, Q.Subdiag(Theta, i))
  }
  
  Q <- Matrix::bandSparse(2^n, k = -2^(0 : (n-1)), diagonals=Subdiags)
  diag(Q) <- -colSums(Q)
  
  return(Q)
}


#########

Build.Q.Extended <- function(Theta, Omega) {
  n <- nrow(Theta)
  Q <- Build.Q(Theta)
  
  # Crear una matriz binaria que representa todos los estados posibles (2^n filas, n columnas)
  states <- matrix(rep(0:(2^n - 1), each = n), ncol = n)
  states <- t(apply(states, 1, function(x) as.integer(intToBits(x)[1:n])))  # Binario en forma de matriz
  
  # Vector de productos Omega, cada fila corresponderá a un estado
  omega_products <- apply(states, 1, function(state) prod(Omega[which(state == 1)]))
  
  # Crear la matriz U de manera eficiente
  U <- Matrix(0, nrow = 2^n, ncol = 2^n)
  U[cbind(1:(2^n), 1:(2^n))] <- omega_products  # Asignar los productos en la diagonal
  
  # Calcular T
  T <- Q - U
  
  # Crear la matriz Q_bar de tamaño 2^(n+1)
  Q_bar <- Matrix(0, nrow = 2^(n+1), ncol = 2^(n+1))
  Q_bar[1:2^n, 1:2^n] <- T
  Q_bar[1:2^n, (2^n+1):(2^(n+1))] <- U
  
  return(Q_bar)
}

########

#Get the diagonal of Q.
##No change
Q.Diag <- function(Theta){
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n){
    dg <- dg - Q.Subdiag(Theta, i)
  }
  
  return(dg)
}

#############

