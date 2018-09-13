library(backtestPortfolio)
library(xts)


# load data
data(prices)


# perform backtesting
my_path <- "d:/Users/rzhouae/Documents/R/Git/backtestPortfolio/R_buildignore/students fuction"
my_mac_path <- "/Users/zhourui/Documents/R/GitProjects/backtestPortfolio/R_buildignore/students fuction/"

res <- multiplePortfolioFunEval(path = my_path, prices = prices[1:5], freq_optim = 100)
