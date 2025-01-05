library(Matrix)

# Función para generar Theta y Omega aleatorios
Random.Theta.Omega <- function(n, sparsity = 0) {
  Theta <- matrix(0, nrow = n, ncol = n)
  
  diag(Theta) <- rnorm(n)
  nonZeros <- sample(which(lower.tri(Theta) | upper.tri(Theta)), 
                     size = floor((n^2 - n) * (1 - sparsity)))
  Theta[nonZeros] <- rnorm(length(nonZeros))
  
  Omega <- exp(rnorm(n))
  
  return(list(Theta = round(Theta, 2), Omega = round(Omega, 2)))
}

# Función para construir la subdiagonal de Q
Q.Subdiag <- function(Theta, i) {
  row <- Theta[i,]
  n <- length(row)
  
  s <- exp(row[i])
  for (j in 1:n) {
    s <- c(s, s * exp(row[j]) * (i != j))
  }
  
  return(s)
}

# Función para construir la matriz Q
Build.Q <- function(Theta) {
  n <- nrow(Theta)
  
  Subdiags <- matrix(0, nrow = 2^n, ncol = n)
  for (i in 1:n) {
    Subdiags[, i] <- Q.Subdiag(Theta, i)
  }
  
  Q <- bandSparse(2^n, k = -2^(0:(n-1)), diagonals = Subdiags)
  diag(Q) <- -rowSums(Q)
  
  return(Q)
}

# Función para construir la matriz Q extendida
Build.Q.Extended <- function(Theta, Omega) {
  n <- nrow(Theta)
  Q <- Build.Q(Theta)
  
  # Creamos una matriz U que aplica Omega a todos los estados
  U <- Matrix(0, nrow = 2^n, ncol = 2^n)
  for (i in 0:(2^n - 1)) {
    state <- as.integer(intToBits(i)[1:n])
    omega_product <- prod(Omega[which(state == 1)])
    U[i + 1, i + 1] <- omega_product
  }
  
  # Creamos una Matriz T
  T <- Q - U
  
  # Construimos Q̄
  Q_bar <- Matrix(0, nrow = 2^(n+1), ncol = 2^(n+1))
  Q_bar[1:2^n, 1:2^n] <- T
  Q_bar[1:2^n, (2^n+1):(2^(n+1))] <- U
  
  return(Q_bar)
}


###########################################
##########################################
##########################################
# Ejemplo de uso
set.seed(123)  # Para reproducibilidad
n <- 3  # Número de eventos
result <- Random.Theta.Omega(n)
Theta <- result$Theta
Omega <- result$Omega

cat("Matriz Theta:\n")
print(Theta)

cat("\nVector Omega:\n")
print(Omega)

Q_bar <- Build.Q.Extended(Theta, Omega)

cat("\nMatriz Q extendida (Q_bar):\n")
print(as.matrix(Q_bar))

Learn.Indep <- function(pD){
  n <- log(length(pD), base=2)        # Número de eventos
  Theta <- matrix(0, nrow=n, ncol=n)  # Inicializa Theta
  
  for(i in 1:n){
    pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)    
    
    perc <- sum(pD[,2])               # Proporción de los eventos observados
    Theta[i,i] <- log(perc/(1-perc))
  }
  
  return(round(Theta,2)) # Retorna la matriz Theta ajustada
}

# Learn.Indep modificado para incorporar Omega:
Learn.Indep.WithOmega <- function(pD){
  n <- log(length(pD), base=2)        # Número de eventos
  Theta <- matrix(0, nrow=n, ncol=n)  # Inicializa Theta
  Omega <- rep(1, n)                  # Inicializa Omega con valores neutros (1)
  
  for(i in 1:n){
    pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)    
    
    perc <- sum(pD[,2])               # Proporción de los eventos observados
    Theta[i,i] <- log(perc/(1-perc))
    
    # Inicializa Omega[i] basado en heurística
    # Por ejemplo, podemos usar la frecuencia relativa del evento.
    Omega[i] <- perc + 1  # Añadimos 1 para asegurar que Omega > 0
  }
  
  # Retorna la matriz Theta ajustada y Omega
  return(list(Theta = round(Theta,2), Omega = round(Omega,2)))
}

# Función para crear una subdiagonal de Q a partir de la línea ith de Theta.
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

# Para obtener la diagonal de Q.
Q.Diag <- function(Theta, Omega = rep(1, ncol(Theta))){
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n){
    # Incorpora Omega al calcular la subdiagonal
    dg <- dg - Q.Subdiag(Theta, i) * exp(log(Omega[i])) # Aplica el sesgo de Omega
  }
  
  return(dg)
}