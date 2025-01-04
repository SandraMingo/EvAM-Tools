################################################3
################################################
################################################
#Estas modificaciones hacen que omega ajuste las tasas de transición 
#y las distribuciones estacionarias, incorporando correcciones de sesgo.
#El código ahora puede trabajar tanto con cMHN (usando omage = 1) o 
# como con oMHN (usando valores personalizados de omega)



#El vector Ω ajusta las tasas de transición, por lo que debe multiplicar 
# o afectar los términos dependientes de Θ. 
#Incluir omega como parametro

Q.vec <- function(Theta, x, Omega = rep(1, ncol(Theta)), diag = FALSE, transp = FALSE){
  n <- ncol(Theta)
  y <- rep(0, 2^n)
  
  for(i in 1:n){ # Ajustar cada fila con Omega
    y <- y + kronvec(exp(Theta[i,] + log(Omega[i])), i, x, diag, transp)
  }
  
  return(y)
}

#Incluir omega como parametro

Jacobi <- function(Theta, b, Omega = rep(1, ncol(Theta)), transp = FALSE, x = NULL){
  n <- ncol(Theta)
  if(is.null(x)) x <- rep(1, 2^n) / (2^n)
  
  dg <- -Q.Diag(Theta) + 1  # No afecta la diagonal directamente
  
  for(i in 1:(n+1)){
    x <- b + Q.vec(Theta, x, Omega = Omega, diag = FALSE, transp = transp)
    x <- x / dg
  }
  
  return(x)
}

#Incluir omage como parametro

Generate.pTh <- function(Theta, Omega = rep(1, ncol(Theta)), p0 = NULL){
  n <- ncol(Theta)
  if(is.null(p0)) p0 <- c(1, rep(0, 2^n - 1))
  
  return(Jacobi(Theta, p0, Omega = Omega))
}


#Iincluir omega como parametro

Score <- function(Theta, pD, Omega = rep(1, ncol(Theta))){
  pTh <- Generate.pTh(Theta, Omega = Omega)
  as.numeric(pD %*% log(pTh))
}


# Inlcuir omega como parametro:

Grad <- function(Theta, pD, Omega = rep(1, ncol(Theta))){
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow = n, ncol = n)
  
  p0 <- c(1, rep(0, 2^n - 1))
  
  pTh <- Jacobi(Theta, p0, Omega = Omega)
  q <- Jacobi(Theta, pD / pTh, Omega = Omega, transp = TRUE)
  
  G <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n){ # Incorporar Omega en el gradiente
    r <- q * kronvec(exp(Theta[i,] + log(Omega[i])), i, pTh, 1, 0)
    G[i,] <- grad_loop_j(i, n, r)
  }
  
  return(G)
}


##Asegurarse de que kronvec o Q.Diag esten modificadas
kronvec <- function(Theta, i_, x, diag_, transp_, Omega = rep(1, nrow(Theta))){
  # Ajustar Theta globalmente con Omega
  Theta_adjusted <- Theta + log(Omega)
  
  # Llamar a la función C con Theta ajustado
  .Call(C_kronvec, Theta_adjusted, i_, x, diag_, transp_, PACKAGE = "evamtools")
}




Q.Diag <- function(Theta, Omega = rep(1, ncol(Theta))){
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n){
    # Incorporar Omega al calcular la subdiagonal
    dg <- dg - Q.Subdiag(Theta, i) * exp(log(Omega[i])) # Aplica el sesgo de Omega
  }
  
  return(dg)
}

