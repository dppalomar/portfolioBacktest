# to verify that both parallel evaluation mode can be performed simultaneously

library(portfolioBacktest)
data("dataset10") 

# define uniform portfolio
uniform_portfolio_fun <- function(dataset, prices = dataset$adjusted) {
  return(rep(1/ncol(prices), ncol(prices)))
}

# define GMVP
GMVP_portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # it may not satisfy w>=0
  return(w)
}

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

portfolios <- list("Uniform" = uniform_portfolio_fun,
                   "GMVP"    = GMVP_portfolio_fun,
                   "Mark"    = Markowitz_portfolio_fun)


bt_paral_datasets <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_datasets = 5,
                                       shortselling = TRUE, leverage = Inf, 
                                       return_portfolio = TRUE, return_return = TRUE, 
                                       benchmark = c("uniform", "index"), show_progress_bar = TRUE,
                                       T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

bt_paral_datasets_ <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_datasets = 5, paral_portfolios = 2,
                                       shortselling = TRUE, leverage = Inf, 
                                       return_portfolio = TRUE, return_return = TRUE, 
                                       benchmark = c("uniform", "index"), show_progress_bar = TRUE,
                                       T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

backtestSummary(bt_paral_datasets)$performance
backtestSummary(bt_paral_datasets_)$performance
