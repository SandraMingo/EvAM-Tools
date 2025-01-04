
Random.Theta <- function(n, sparsity=0){
  Theta  <- matrix(0,nrow=n,ncol=n)
  
  diag(Theta)  <- NA
  nonZeros <- sample(which(!is.na(Theta)), size=(n^2 - n)*(1 - sparsity))
  
  Theta[nonZeros] <- rnorm(length(nonZeros))
  diag(Theta) <- rnorm(n)
  
  return(round(Theta,2))
} 


# Función para crear una subdiagonal a partir de Theta
Q.Subdiag <- function(Theta, i){
  row <- Theta[i,]
  n <- length(row)
  
  # Empieza la subdiagonal con la tasa base de Theta_ii
  s <- exp(row[i])
  
  # Duplica para cada factor Theta_ij, exceptuando cuando i = j
  for(j in 1:n){
    s <- c(s, s * exp(row[j]) * (i != j))  # Evita duplicar para j == i
  }
  
  return(s)
}


Q.Diag <- function(Theta){
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n){
    dg <- dg - Q.Subdiag(Theta, i)
  }
  
  return(dg)
}


#Ajustada

Build.Q <- function(Theta, Omega, num_diagonals = 3) {
  n <- nrow(Theta)
  num_states <- 2^n
  
  # Calculamos el rango de k basado en num_diagonals
  k_range <- -floor(num_diagonals/2):floor(num_diagonals/2)
  
  # Inicializamos las diagonales
  diagonals <- vector("list", length(k_range))
  for(i in seq_along(k_range)) {
    diagonals[[i]] <- rep(0, num_states)
  }
  
  # Llenamos las diagonales con los valores de Subdiags
  for(i in 1:n) {
    subdiag <- Q.Subdiag(Theta, i)
    for(j in seq_along(k_range)) {
      k <- k_range[j]
      if(k < 0) {
        diagonals[[j]] <- diagonals[[j]] + c(rep(0, 2^(i-1)), subdiag[1:(num_states-2^(i-1))])
      } else if(k > 0) {
        diagonals[[j]] <- diagonals[[j]] + c(subdiag[(2^(i-1)+1):num_states], rep(0, 2^(i-1)))
      } else {
        diagonals[[j]] <- diagonals[[j]] + subdiag[1:num_states]
      }
    }
  }
  
  # Usamos Matrix::bandSparse para construir la matriz de transición
  Q <- Matrix::bandSparse(num_states, k = k_range, diagonals = diagonals)
  
  # El resto del código permanece igual...
  U <- Matrix::Diagonal(n = num_states, x = c(rep(1, n), Omega))
  T <- Q - U
  Q_extended <- Matrix::bdiag(T, U)
  diag(Q_extended) <- -colSums(Q_extended)
  
  return(Q_extended)
  
  
  Learn.Indep <- function(pD){
    n <- log(length(pD), base=2)
    Theta <- matrix(0, nrow=n, ncol=n)
    
    for(i in 1:n){
      pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)    
      
      perc <- sum(pD[,2])
      Theta[i,i] <- log(perc/(1-perc))
    }
    
    return(round(Theta,2))
  }
}


#Ajuatada

Learn.Indep <- function(pD, Omega) {
  n <- log(length(pD), base=2)  # Número de eventos
  Theta <- matrix(0, nrow=n, ncol=n)  # Inicializa Theta
  
  for(i in 1:n) {
    pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)    
    perc <- sum(pD[, 2])  # Proporción de los eventos observados
    
    # Ajustamos Theta con el sesgo de observación
    Theta[i, i] <- log(perc / (1 - perc)) + log(Omega[i])  # Ajuste con Omega
  }
  
  return(round(Theta, 2))  # Retorna la matriz Theta ajustada
}