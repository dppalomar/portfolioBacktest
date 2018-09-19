library(portfolioBacktest)
library(xts)


# load data
data(prices)


# perform backtesting
my_path <- "d:/Users/rzhouae/Documents/R/Git/portfolioBacktest/R_buildignore/student-functions/"
my_mac_path <- "/Users/zhourui/Documents/R/GitProjects/backtestPortfolio/R_buildignore/students fuction/"

res <- multiplePortfolioBacktest(folder_path = my_path, prices = prices[1:3], optimize_every = 100)


library(portfolioBacktest)
library(xts)
data(prices)
uniform_portfolio_fun <- function(prices) {
  N <- ncol(prices)
  w <- rep(1/N, N)  # satisfies the constraints w>=0 amd sum(w)=1
  return(w)
}

GMVP_portfolio_fun <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # satisfies sum(w)=1 but not necessarily w>=0
  return(w)
}

portfolio_function_list <- c(GMVP_portfolio_fun, uniform_portfolio_fun)


res <- multiplePortfolioBacktest(portfolio_fun_list = portfolio_function_list, 
                                 prices = prices[1:3], shortselling = TRUE)
str(res)

names(portfolio_function_list) <- c("GMVP", "uniform")
res <- multiplePortfolioBacktest(portfolio_fun_list = portfolio_function_list, 
                                 prices = prices[1:3], shortselling = TRUE)
str(res)

res <- multiplePortfolioBacktest(portfolio_fun_list = portfolio_function_list, 
                                 prices = prices[1:3], shortselling = TRUE, 
                                 return_portfolio = TRUE, return_all = TRUE)
str(res)
