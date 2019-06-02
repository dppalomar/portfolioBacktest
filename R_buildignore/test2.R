library(portfolioBacktest)
data("dataset10")  # load dataset

# define your own portfolio function
uniform_portfolio <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}

# do backtest
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), dataset10)
