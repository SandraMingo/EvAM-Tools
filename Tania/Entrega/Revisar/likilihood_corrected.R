
## Modificación de Jacobi:

Jacobi <- function(Theta, Omega, b, transp=F, x=NULL){
  Q_bar <- Build.Q.Extended(Theta, Omega)  # Matriz extendida
  n <- nrow(Theta)
  size <- 2^(n + 1)  # Dimensión de Q_bar
  if (is.null(x)) x <- rep(1, size) / size
  
  dg <- -diag(Q_bar) + 1  # Diagonal de -Q_bar + I
  
  for (i in 1:(n + 1)){
    x <- b + Q_bar %*% x  # Producto matriz-vector con la extendida
    x <- x / dg
  }
  
  return(x)
}

Generate.pTh <- function(Theta, Omega, p0 = NULL){
  n <- ncol(Theta)
  size <- 2^(n + 1)  # Tamaño de la matriz extendida
  if (is.null(p0)) p0 <- c(1, rep(0, size - 1))  # Distribución inicial extendida
  
  return(Jacobi(Theta, Omega, p0))
}

Score <- function(Theta, Omega, pD){
  pTh <- Generate.pTh(Theta, Omega)
  as.numeric(pD %*% log(pTh))
}

Grad <- function(Theta, Omega, pD){
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow=n, ncol=n)
  
  p0 <- c(1, rep(0, 2^(n + 1) - 1))  # Distribución inicial extendida
  
  pTh <- Jacobi(Theta, Omega, p0)
  q <- Jacobi(Theta, Omega, pD / pTh, transp=T)
  
  G <- matrix(0, nrow=n, ncol=n)
  for(i in 1:n){ # Paralelización opcional
    r <- q * kronvec(exp(Theta[i,]) * Omega[i], i, pTh, 1, 0)
    G[i,] <- grad_loop_j(i, n, r)
  }
  
  return(G)
}
