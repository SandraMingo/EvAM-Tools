require("Matrix")

Random.Theta <- function(n, sparsity=0){
  Theta  <- matrix(0,nrow=n,ncol=n)
  
  diag(Theta)  <- NA
  nonZeros <- sample(which(!is.na(Theta)), size=(n^2 - n)*(1 - sparsity))
  
  Theta[nonZeros] <- rnorm(length(nonZeros))
  diag(Theta) <- rnorm(n)
  
  return(round(Theta,2))
} 

Random.Theta.Omega <- function(n, sparsity = 0) {
  Theta <- Random.Theta(n, sparsity)
  
  omega_Theta <- matrix(0, nrow = n + 1, ncol = n)
  omega_Theta[1:n, ] <- Theta
  
  return(round(omega_Theta, 2))
}


Remove.Last.Row <- function(matrix) {
  return(matrix[1:(nrow(matrix) - 1), ])
}


# Create a single subdiagonal of Q from the omega_Theta matrix.
Q.Subdiag <- function(Theta, i){
  row <- Theta[i,]
  n <- length(row)
  
  s <- exp(row[i])
  
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

  diag(Q) <- -Matrix::colSums(Q)
  
  Q_extended <- Matrix::bdiag(Q, extra_row)
  
  return(Q_extended)
}

#Get the diagonal of Q.
Q.Diag <- function(omega_Theta) {
  Theta <- Remove.Last.Row(omega_Theta)
  
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n) {
    dg <- dg - Q.Subdiag(Theta, i)
  }
  
  return(dg)
}

####################################################
########################TEST########################
####################################################

# Generar omega_Theta
omega_Theta <- Random.Theta.Omega(n = 2, sparsity = 0.3)

Theta <- Remove.Last.Row(omega_Theta)

# Construir la matriz Q
Q_matrix <- Build.Q.Extended(omega_Theta)

# Ver la matriz resultante Q
print(Q_matrix)
