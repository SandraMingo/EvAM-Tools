## require("Matrix")

#Create a random MHN with (log-transformed) parameters Theta and Omega.
#Sparsity is given as percentage.
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

# Create a single subdiagonal of Q from the with row in Theta.
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

# Learn.Indep modificado para incorporar Omega:
Learn.Indep.Omega <- function(pD, Omega){
  n <- log(length(pD), base=2)
  Theta <- matrix(0, nrow=n, ncol=n)
  
  # Reorganizar pD según el número de eventos n (se hace un reshape)
  pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)  
  
  for(i in 1:n){
    # Calcular la probabilidad de que el evento i haya ocurrido (dependiente de Omega)
    perc <- sum(pD[, 2]) * Omega[i]  # Incorporando Omega en el cálculo
    
    # La tasa de transición Theta[i, i] depende de Omega y la probabilidad de ocurrencia
    Theta[i, i] <- log(perc / (1 - perc))
  }
  
  return(round(Theta, 2))
}

