# This file is only to store some benchmark portfolios

# uniform portfolio function
uniform_portfolio_fun <- function(prices) {
  N <- ncol(prices)
  return(rep(1/N, N))
}