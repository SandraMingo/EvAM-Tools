## require("Matrix")

Random.Theta <- function(n, sparsity=0){
  Theta  <- matrix(0,nrow=n,ncol=n)
  
  diag(Theta)  <- NA
  nonZeros <- sample(which(!is.na(Theta)), size=(n^2 - n)*(1 - sparsity))
  
  Theta[nonZeros] <- rnorm(length(nonZeros))
  diag(Theta) <- rnorm(n)
  
  return(round(Theta,2))
} 

Random.Theta.Omega <- function(n, sparsity = 0) {
  # Obtener la matriz Theta de la función anterior
  Theta <- Random.Theta(n, sparsity)
  
  # Crear la matriz omega_Theta (de tamaño n+1 por n) con ceros
  omega_Theta <- matrix(0, nrow = n + 1, ncol = n)
  
  # Copiar Theta en las primeras n filas de omega_Theta
  omega_Theta[1:n, ] <- Theta
  
  # Devolver la matriz omega_Theta
  return(round(omega_Theta, 2))
}


Remove.Last.Row <- function(matrix) {
  # Elimina la última fila de la matriz
  return(matrix[1:(nrow(matrix) - 1), ])
}


# Create a single subdiagonal of Q from the omega_Theta matrix.
Q.Subdiag <- function(Theta, i){
  row <- Theta[i,]
  n <- length(row)
  
  #start the subdiagonal with the base rate Theta_ii 
  s <- exp(row[i])
  
  #and duplicate it for each additional factor Theta_ij.
  for(j in 1:n){
    s <- c(s, s * exp(row[j]) * (i != j))
  }
  
  return(s)
}


#Build the transition rate matrix Q from its subdiagonals.
Build.Q.Extended <- function(omega_Theta){
  extra_row <- omega_Theta[nrow(omega_Theta), ]
  
  n <- nrow(Theta)
  
  Subdiags <- c()
  for(i in 1:n){
    Subdiags <- cbind(Subdiags, Q.Subdiag(Theta, i))
  }
  
  Q <- Matrix::bandSparse(2^n, k = -2^(0 : (n-1)), diagonals=Subdiags)

  diag(Q) <- -Matrix::colSums(Q)  # Para matrices dispersas
  
  # Crear la matriz extendida Q_extended
  # Añadir la fila extra a la matriz Q
  Q_extended <- Matrix::bdiag(Q, extra_row)
  
  return(Q_extended)
}

#Build.Q <- function(omega_Theta) {
#  # Eliminar la última fila de omega_Theta para trabajar solo con Theta
#  Theta <- Remove.Last.Row(omega_Theta)
#  
#  n <- nrow(Theta)
#  
#  Subdiags <- NULL
#  for(i in 1:n){
#    subdiag_i <- Q.Subdiag(Theta, i)

#    Subdiags <- cbind(Subdiags, subdiag_i)
#  }
  
#  Q <- Matrix::bandSparse(n, k = -1, diagonals = Subdiags)
#  diag(Q) <- -colSums(Q)
  
#  return(Q)
#}


########

#Get the diagonal of Q.
Q.Diag <- function(omega_Theta) {
  # Eliminar la última fila de omega_Theta para trabajar solo con Theta
  Theta <- Remove.Last.Row(omega_Theta)
  
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n) {
    dg <- dg - Q.Subdiag(Theta, i)
  }
  
  return(dg)
}

######################################################
# Generar omega_Theta
omega_Theta <- Random.Theta.Omega(n = 2, sparsity = 0.3)

Theta <- Remove.Last.Row(omega_Theta)

# Construir la matriz Q
Q_matrix <- Build.Q.Extended(omega_Theta)

# Ver la matriz resultante Q
print(Q_matrix)
