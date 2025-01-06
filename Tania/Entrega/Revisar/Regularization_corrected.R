L1 <- function(Theta, Omega, eps=1e-05) {
  diag(Theta) <- 0
  sum(sqrt((Theta * Omega)^2 + eps))
}

L1_ <- function(Theta, Omega, eps=1e-05) {
  diag(Theta) <- 0
  (Theta * Omega) / sqrt((Theta * Omega)^2 + eps)
}

Score.Reg <- function(Theta, pD, lambda, Omega) {
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow=n, ncol=n)  
  Score(Theta, pD) - lambda * L1(Theta, Omega)
}

Grad.Reg <- function(Theta, pD, lambda, Omega) {
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow=n, ncol=n)  
  Grad(Theta, pD) - lambda * L1_(Theta, Omega)
}

Learn.MHN <- function(pD, init=NULL, lambda=0, Omega=NULL, maxit=5000, trace=0, reltol=1e-07, round=T) {
  n <- log(length(pD), base=2)
  
  if (is.null(init)) {
    init <- Learn.Indep(pD)
  }
  
  opt <- optim(init, fn=Score.Reg, gr=Grad.Reg, pD, lambda, Omega, method="BFGS", 
               control=list(fnscale=-1, trace=trace, maxit=maxit, reltol=reltol))
  
  Theta <- matrix(opt$par, nrow=n, ncol=n)
  
  if (round) {
    Theta <- round(Theta, 2)
  }
  
  return(Theta)
}

#######################################################
#######################################################
#######3con lo de python incluido
######################333

# Smooth approximation of the L1 penalty on Theta.
# (to be replaced with OWL-QN)

L1 <- function(Theta, Omega, eps=1e-05) {
  diag(Theta) <- 0
  Theta <- Theta * Omega  # Aplica Omega como factor multiplicativo
  sum(sqrt(Theta^2 + eps))
}

# Derivative of L1 penalty
L1_ <- function(Theta, Omega, eps=1e-05) {
  diag(Theta) <- 0
  Theta <- Theta * Omega  # Aplica Omega como factor multiplicativo
  Theta / sqrt(Theta^2 + eps)
}

# Regularized Score 
Score.Reg <- function(Theta, pD, lambda, Omega) {
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow=n, ncol=n)  
  Score(Theta, pD) - lambda * L1(Theta, Omega)
} 

# Regularized Gradient
Grad.Reg  <- function(Theta, pD, lambda, Omega) {
  n <- sqrt(length(Theta))
  Theta <- matrix(Theta, nrow=n, ncol=n)  
  Grad(Theta, pD) - lambda * L1_(Theta, Omega)
}

# Learn an MHN from data.
Learn.MHN <- function(pD, init=NULL, lambda=0, Omega=NULL, maxit=5000, trace=0, reltol=1e-07, round=T) {
  n <- log(length(pD), base=2)
  
  # Initialize the parameters from the independence model
  if (is.null(init)) {
    init <- Learn.Indep.Omega(pD, Omega)
  } 
  
  opt <- optim(init, fn=Score.Reg, gr=Grad.Reg, pD, lambda, Omega, method="BFGS", 
               control=list(fnscale=-1, trace=trace, maxit=maxit, reltol=reltol))
  
  Theta <- matrix(opt$par, nrow=n, ncol=n)
  
  if (round) {
    Theta <- round(Theta, 2)
  }
  
  return(Theta)
}


### Uso

# Ejemplo de uso:
Omega <- rep(1.5, 10)  # Un vector Omega de tamaño 10 (ajuste multiplicativo)
pD <- matrix(rnorm(n^2), nrow=n, ncol=n)         # Tus datos de entrada
init <- NULL             # O puedes proporcionar una inicialización específica

set.seed(42)  # Para reproducibilidad
n <- 5  # Número de variables o nodos
pD <- matrix(rnorm(n^2), nrow=n, ncol=n)  # Simula una matriz de datos de tamaño n x n
Omega <- runif(n, 0.5, 2)  # Omega como un vector con valores aleatorios entre 0.5 y 2

# Llamada a Learn.MHN con estos datos
init <- NULL  # Dejar que la función use la inicialización predeterminada
lambda <- 0.1  # Un valor de lambda (regularización)
Theta <- Learn.MHN(pD, init, lambda, Omega, maxit=5000, trace=1)

# Mostrar los resultados
print(Theta)

# Llamada a Learn.MHN con Omega
Theta <- Learn.MHN(pD, init, lambda=0.1, Omega=Omega, maxit=1000)
