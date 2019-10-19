# This file is only to store some benchmark portfolios

# uniform portfolio function
uniform_portfolio_fun <- function(data) {
  N <- ncol(data$adjusted)
  return(rep(1/N, N))
}

# inverse-volatility portfolio function
#' @importFrom stats cov
IVP_portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]
  sigma <- sqrt(diag(cov(X)))
  w <- 1/sigma
  return(w/sum(w))
}

# benchmark library
benchmark_library <- list(
  "uniform" = uniform_portfolio_fun,
  "IVP"     = IVP_portfolio_fun
)
