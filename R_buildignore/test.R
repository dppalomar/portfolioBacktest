library(backtestPortfolio)
library(xts)
library(CVXR)

# load data
data(prices)

# define some portfolio functions
portfolio_fun_Markowitz <- function(prices) {
  # compute log returns
  X <- diff(log(prices))[-1]

  # compute mean vector and SCM
  mu <- colMeans(X)
  Sigma <- cov(X)

  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

portfolio_fun_GMVP <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # normalized to have ||w||_1=1
  return(w)
}




# perform single backtesting
res <- backtestPortfolio(portfolio_fun_GMVP, prices[[1]], shortselling = TRUE)
#res <- backtestPortfolio(portfolio_fun_Markowitz, prices[[1]])
names(res)
plot(res$returns)
plot(res$cumPnL)
PerformanceAnalytics::chart.CumReturns(res$returns, geometric = FALSE, wealth.index = TRUE)
res$performance
PerformanceAnalytics::table.AnnualizedReturns(res$returns)
PerformanceAnalytics::charts.PerformanceSummary(res$returns, wealth.index = TRUE)

# perform multiple backtesting
mul_res <- backtestPortfolio(portfolio_fun_Markowitz, prices[1:10])
mul_res$performance
mul_res$performance_summary

