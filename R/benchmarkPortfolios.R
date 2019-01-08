# This file is only to store some benchmark portfolios

# uniform portfolio function
uniform_portfolio_fun <- function(data) {
  N <- ncol(data$adjusted)
  return(rep(1/N, N))
}