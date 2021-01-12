# This is to generate the simulation result for vignettes
# using 100 resampled datasets

rm(list = ls())

library(portfolioBacktest)
# data(SP500_symbols)
# 
# SP500_YAHOO <- stockDataDownload(stock_symbols = SP500_symbols,
#                                 from = "2008-12-01", to = "2018-12-01")
# save(SP500_YAHOO, file = "data-raw/SP500_YAHOO.RData")

load("data-raw/SP500_YAHOO.RData")

my_dataset_list <- financialDataResample(SP500_YAHOO, N = 50, T = 252*2, num_datasets = 100)

# define quintile portfolio
quintile_portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]  # compute log returns
  N <- ncol(X)
  # design quintile portfolio
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}

# define GMVP (allowing shorting)
GMVP_portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))
  return(w)
}

# define Markowitz mean-variance portfolio
library(CVXR)
Markowitz_portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]  # compute log returns
  mu <- colMeans(X)  # compute mean vector
  Sigma <- cov(X)  # compute the SCM
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

portfolios <- list("Quintile"  = quintile_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun)
bt <- portfolioBacktest(portfolios, my_dataset_list, benchmark = c("uniform", "index"), 
                        paral_datasets = 4, show_progress_bar = TRUE,
                        return_portfolio = FALSE, return_return = FALSE)


res_sum <- backtestSummary(bt)
summaryTable(res_sum, type = "DT")

summaryBarPlot(res_sum, measures = c("Sharpe ratio", "max drawdown"), type = "ggplot2")

backtestBoxPlot(bt, measure = "Sharpe ratio")
backtestBoxPlot(bt, measure = "Max drawdown")

save(bt, file = "vignettes/figures/bt.RData", compression_level = 9)
