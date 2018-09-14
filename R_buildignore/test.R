library(backtestPortfolio)
library(xts)

# library(CVXR)
# naivePortfolioDesign <- function(prices) {
#   # compute log returns
#   X <- diff(log(prices))[-1]
#   
#   # compute mean vector and SCM
#   mu <- colMeans(X)
#   Sigma <- cov(X)
#   
#   # design mean-variance portfolio
#   w <- Variable(nrow(Sigma))
#   prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
#                   constraints = list(w >= 0, sum(w) == 1))
#   result <- solve(prob)
#   return(as.vector(result$getValue(w)))
# }


# load data
data(prices)

# define portfolio function
portfolio_fun <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(w)
  return(w)
}

# perform backtesting
res <- backtestPortfolio(portfolio_fun, prices[[1]])
mul_res <- backtestPortfolio(portfolio_fun, prices[1:3])

# performance
names(res)
plot(res$returns)
plot(res$cumPnL)
PerformanceAnalytics::chart.CumReturns(res$returns, geometric = FALSE, wealth.index = TRUE)
PerformanceAnalytics::table.AnnualizedReturns(res$returns)
PerformanceAnalytics::charts.PerformanceSummary(res$returns, wealth.index = TRUE)
