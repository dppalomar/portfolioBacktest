library(backtestPortfolio)
library(xts)


# load data
data(prices)


# perform backtesting
my_path <- "d:/Users/rzhouae/Documents/R/Git/backtestPortfolio/R_buildignore/students fuction"
res <- multiplePortfolioFunEval(path = my_path, prices = prices[1:3], freq_optim = 50)
