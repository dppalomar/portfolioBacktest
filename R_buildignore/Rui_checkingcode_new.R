# define some portfolio functions
portfolio_fun_Markowitz <- function(prices) {
  require(CVXR)
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

portfolio_fun_uniform <- function(prices) {
  return(rep(1/ncol(prices), ncol(prices)))
}


library(portfolioBacktest)
data("dataset")

tmp <- portfolioBacktest:::singlePortfolioBacktest(portfolio_fun_uniform, dataset, show_progress_bar = FALSE)
portfolio_funs <- list(my_uniform = portfolio_fun_uniform,
                       my_GMVP = portfolio_fun_GMVP,
                       my_GMVP_norm = portfolio_fun_GMVP_norm,
                       my_Markowitz = portfolio_fun_Markowitz)

res1_mute <- portfolioBacktest(portfolio_funs, dataset, benchmark = c('uniform', 'index'))
res2_mute <- portfolioBacktest(portfolio_funs, dataset, benchmark = c('index'), shortselling = TRUE)

res1_bar <- portfolioBacktest(portfolio_funs, dataset, benchmark = c('uniform', 'index'), show_progress_bar = TRUE)
res2_bar <- portfolioBacktest(portfolio_funs, dataset, benchmark = c('index'), shortselling = TRUE, show_progress_bar = TRUE)

names(res1)
names(res2)

backtestSelector(res1_mute, portfolio_name = "my_GMVP")$performance
backtestSelector(res2_mute, portfolio_name = "my_GMVP")$performance

backtestSelector(res1_mute, portfolio_name = "my_GMVP_norm")$error_message
backtestSelector(res2_mute, portfolio_name = "my_GMVP_norm")$error_message

### test function: backtestSelector ############
backtestSelector(res1, portfolio_name = 'uniform', selector = c('Sharpe ratio', 'max drawdown'))

backtestSummary(res2_mute, portfolio_index = 2)
backtestSummary(res2_mute, portfolio_indexs = 1:3)
portfolioLeaderboard(res2_mute, weights = list('Sharpe ratio' = 1,
                                               'max drawdown' = 1,
                                               'annual return' = 1,
                                               'failure rate' = 7))
