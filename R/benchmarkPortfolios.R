# This file is only to store some benchmark portfolios

# 1/N portfolio function
EWP_portfolio_fun <- function(data, ...) {
  N <- ncol(data[[1]])
  return(rep(1/N, N))
}

# inverse-volatility portfolio function
#' @importFrom stats cov
IVP_portfolio_fun <- function(data, ...) {
  X <- diff(log(data[[1]]))[-1]
  sigma <- sqrt(diag(cov(X)))
  w <- 1/sigma
  w <- w/sum(w)
  
  leverage <- parent.frame(n = 1)$leverage
  if (is.null(leverage) || is.infinite(leverage))
    return(w)
  else
    return(w * leverage)
}

# Global Minimum Variance Portfolio
#' @importFrom quadprog solve.QP
GMVP <- function(data, shrinkage = FALSE, ...) {
  shortselling <- parent.frame(n = 2)$shortselling  # inherit shortselling from grandparent environment
  if (is.null(shortselling)) shortselling <- FALSE
  
  leverage <- parent.frame(n = 2)$leverage  # inherit shortselling from grandparent environment
  if (is.null(leverage)) leverage <- Inf
  
  X <- diff(log(data[[1]]))[-1]
  Sigma <- if (shrinkage) cov_LedoitWolf(X) else cov(X)
  
  if (!shortselling) {
    N <- ncol(Sigma)
    Dmat <- 2 * Sigma
    Amat <- cbind(rep(1, N), diag(N))
    bvec <- c(1, rep(0, N))
    dvec <- rep(0, N)
    w <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)$solution
  } else {
    ones <- rep(1, nrow(Sigma))
    w <- solve(Sigma, ones)  #same as: inv(Sigma) %*% ones
    w <- w/sum(w)  #w <- w/sum(abs(w))  # normalized to have ||w||_1=1
  }
  
  if (leverage < Inf) w <- leverage * w / sum(abs(w))
  return(w)
}


# Markowitz Maximum Sharpe-Ratio Portfolio (MSRP)
MSRP <- function(data, ...) {
  X <- diff(log(data[[1]]))[-1]
  mu <- colMeans(X)
  Sigma <- cov(X)
  w <- MSRP_solver(mu, Sigma)
  return(w)
}

# Most diversified portfolio (MDP)
MDP <- function(data, ...) {
  X <- diff(log(data[[1]]))[-1]
  Sigma <- cov(X)
  mu <- sqrt(diag(Sigma))
  w <- MSRP_solver(mu, Sigma)
  
  leverage <- parent.frame(n = 1)$leverage
  if (is.null(leverage) || is.infinite(leverage))
    return(w)
  else
    return(w / sum(abs(w)) * leverage)
}


# benchmark library
benchmark_library <- list(
  "1/N"              = EWP_portfolio_fun,
  "IVP"              = IVP_portfolio_fun,
  "GMVP"             = function(data, ...) GMVP(data, shrinkage = FALSE, ...),
  "MSRP"             = MSRP,
  "MDP"              = MDP,
  "GMVP + shrinkage" = function(data, ...) GMVP(data, shrinkage = TRUE, ...)
)




#  method for solving Markowitz Maximum Sharpe-Ratio Portfolio (MSRP)
#' @importFrom quadprog solve.QP
MSRP_solver <- function(mu, Sigma) {
  N <- ncol(Sigma)
  if (all(mu <= 1e-8)) return(rep(0, N))
  Dmat <- 2 * Sigma
  Amat <- diag(N)
  Amat <- cbind(mu, Amat)
  bvec <- c(1, rep(0, N))
  dvec <- rep(0, N)
  w <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)$solution
  return(w/sum(w))
}


# methods for parameter estimation
cov_LedoitWolf <- function(X) {
  T <- nrow(X)
  N <- ncol(X)
  T_eff <- T  # T for biased SCM and T-1 for unbiased
  
  Xc <- X - matrix(colMeans(X), T, N, byrow = TRUE)
  S <- crossprod(Xc)/T_eff  # SCM
  Sigma_T <- mean(diag(S)) * diag(N)  # target
  d2 <- sum((S - Sigma_T)^2)
  b2_ <- (1/T^2) * sum(rowSums(Xc^2)^2) - (2*T_eff/T - 1)/T * sum(S^2)
  b2 <- min(b2_, d2)  #a2 = d2 - b2
  rho <- b2/d2
  Sigma_clean <- (1-rho)*S + rho*Sigma_T  #rho=b2/d2, 1-rho=a2/d2
  return(Sigma_clean)
}