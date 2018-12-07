library(CVXR)

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

portfolio_fun_uniform <- function(prices) {
  return(rep(1/ncol(prices), ncol(prices)))
}


library(portfolioBacktest)
data("dataset")

##### test core function: singlePortfolioSingleXTSBacktest ############
res1 <- portfolioBacktest:::singlePortfolioSingleXTSBacktest(portfolio_fun = portfolio_fun_uniform, 
                                                             data = dataset[[1]],
                                                             return_portfolio = TRUE,
                                                             return_return = TRUE)
res2 <- portfolioBacktest:::singlePortfolioSingleXTSBacktest(portfolio_fun = portfolio_fun_GMVP_norm, 
                                                             data = dataset[[1]], shortselling = TRUE)
str(res1)
str(res2)

##### test function: benchmarkBacktest ################
res1 <- portfolioBacktest:::benchmarkBacktest(dataset = dataset, benchmark = c('uniform', 'index'))
str(res1)

##### test function: singlePortfolioBacktest ##########
res1 <- portfolioBacktest:::singlePortfolioBacktest(portfolio_fun = portfolio_fun_uniform,
                                                    dataset = dataset,
                                                    return_portfolio = TRUE,
                                                    return_return = TRUE)
res2 <- portfolioBacktest:::singlePortfolioBacktest(portfolio_fun = portfolio_fun_GMVP_norm,
                                                    dataset = dataset, shortselling = TRUE)
str(res1)
str(res2)
sapply(res1, function(x){x$performance})
sapply(res1, function(x){x$benchmark$uniform$performance})
sapply(res2, function(x){x$performance})
sapply(res2, function(x){x$benchmark$uniform$performance})


##### test function: portfolioBacktest ##########
rm(res1, res2)
res1 <- portfolioBacktest::portfolioBacktest(portfolio_funs = portfolio_fun_uniform,
                                             benchmark = c('uniform', 'index'),
                                             dataset = dataset,
                                             return_portfolio = TRUE,
                                             return_return = TRUE)
res2 <- portfolioBacktest::portfolioBacktest(portfolio_funs = portfolio_fun_GMVP_norm,
                                             benchmark = c(),
                                             dataset = dataset, shortselling = TRUE)
names(res1)
names(res2)
sapply(res1[['fun1']], function(x){x$performance})
sapply(res2[['fun1']], function(x){x$performance})
sapply(res2[['uniform']], function(x){x$performance})


### test function: backtestSelector ############
backtestSelector(res1, portfolio_name = 'uniform', selector = c('Sharpe ratio', 'max drawdown'))

backtestSummary(res1, portfolio_indexs = 1:3)
