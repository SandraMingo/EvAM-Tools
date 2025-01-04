# Create a random MHN with (log-transformed) parameters Theta and Omega.
# Sparsity is given as percentage.
Random.Theta.Omega <- function(n, sparsity=0){
  Theta <- matrix(0, nrow=n, ncol=n)
  
  diag(Theta) <- NA
  nonZeros <- sample(which(!is.na(Theta)), size=(n^2 - n)*(1 - sparsity))
  
  Theta[nonZeros] <- rnorm(length(nonZeros))
  diag(Theta) <- rnorm(n)
  
  # Generate Omega (multiplicative effects on observation rate)
  Omega <- exp(rnorm(n))  # Ensure Omega > 0
  
  return(list(Theta=round(Theta,2), Omega=round(Omega,2)))
}

# Create a single subdiagonal of Q from the ith row in Theta.
Q.Subdiag <- function(Theta, i){
  row <- Theta[i,]
  n <- length(row)
  
  # Start the subdiagonal with the base rate Theta_ii 
  s <- exp(row[i])
  
  # And duplicate it for each additional factor Theta_ij.
  for(j in 1:n){
    s <- c(s, s * exp(row[j]) * (i != j))
  }
  
  return(s)
}

# Build the transition rate matrix Q from its subdiagonals.
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

# Build the extended transition rate matrix Q̄
Build.Q.Extended <- function(Theta, Omega){
  n <- nrow(Theta)
  Q <- Build.Q(Theta)
  
  # Create U matrix
  U <- Matrix::Diagonal(2^n, x=c(1, Omega))
  
  # Create T matrix
  T <- Q - U
  
  # Construct Q̄
  Q_bar <- Matrix::bdiag(T, Matrix::Matrix(0, nrow=2^n, ncol=2^n))
  Q_bar[1:2^n, (2^n+1):(2^(n+1))] <- U
  
  return(Q_bar)
}

# Get the diagonal of Q. 
Q.Diag <- function(Theta){
  n <- ncol(Theta)
  dg <- rep(0, 2^n)
  
  for(i in 1:n){
    dg <- dg - Q.Subdiag(Theta, i)
  }
  
  return(dg)
}

# Learn an independence model from the data distribution, which assumes that no events interact. 
# Used to initialize the parameters of the actual model before optimization.
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

# Learn.Indep modified to incorporate Omega:

Learn.Indep.WithOmega <- function(pD){
  n <- log(length(pD), base=2)
  Theta <- matrix(0, nrow=n, ncol=n)
  Omega <- rep(1, n)  # Initialize Omega with neutral values (1)
  
  for(i in 1:n){
    pD <- matrix(pD, nrow=2^(n-1), ncol=2, byrow=T)    
    
    perc <- sum(pD[,2])
    Theta[i,i] <- log(perc/(1-perc))
    
    # Initialize Omega[i] based on some heuristic
    # For example, we could use the relative frequency of the event
    Omega[i] <- perc + 1  # Adding 1 to ensure Omega > 0
  }
  
  return(list(Theta = round(Theta,2), Omega = round(Omega,2)))
}


# Calculate the stationary distribution
Stationary.Distribution <- function(Theta, Omega, p0){
  n <- nrow(Theta)
  Q <- Build.Q(Theta)
  U <- Matrix::Diagonal(2^n, x=c(1, Omega))
  T <- Q - U
  
  # Calculate the stationary distribution
  p_B_inf <- solve(diag(2^n) - Q %*% solve(U)) %*% p0
  
  return(p_B_inf)
}
