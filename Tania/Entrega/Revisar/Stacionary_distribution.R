# Para calcular la stationary distribution
Stationary.Distribution <- function(Theta, Omega, p0){
  n <- nrow(Theta)
  Q <- Build.Q(Theta)
  U <- Matrix::Diagonal(2^n, x=c(1, Omega))
  T <- Q - U
  
  # Se calcula la Distribución Estacionaria.
  p_B_inf <- solve(diag(2^n) - Q %*% solve(U)) %*% p0
  
  return(p_B_inf)
}


###############################33
