library(xts)
library(portfolioBacktest)
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

portfolio_fun_GMVP_norm <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # normalized to have ||w||_1=1
  return(w)
}

portfolio_fun_GMVP <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(w)  # normalized to have sum(w)=1
  return(w)
}

# perform single backtesting
res <- portfolioBacktest(portfolio_fun_GMVP, prices[[1]])
names(res)
res$error
res$error_message

res <- portfolioBacktest(portfolio_fun_GMVP_norm, prices[[1]])
res$error_message

res <- portfolioBacktest(portfolio_fun_GMVP_norm, prices[[1]], shortselling = TRUE)
res$error

res <- portfolioBacktest(portfolio_fun_Markowitz, prices[[1]], return_portfolio = TRUE,
                         T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
res$performance
res$cpu_time
str(res$portfolio)
res$portfolio[, 48:50]

res <- portfolioBacktest(portfolio_fun_Markowitz, prices[[1]])
res$performance
plot(res$returns)
plot(res$cumPnL)
PerformanceAnalytics::chart.CumReturns(res$returns, geometric = FALSE, wealth.index = TRUE)
PerformanceAnalytics::table.AnnualizedReturns(res$returns)
PerformanceAnalytics::charts.PerformanceSummary(res$returns, wealth.index = TRUE)

# perform multiple backtesting
mul_res <- portfolioBacktest(portfolio_fun_Markowitz, prices[1:3])
mul_res$performance
mul_res$performance_summary

