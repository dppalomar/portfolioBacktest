library(xts)

data(prices)
load("res_returns.RData")

portfolio_fun_uniform <- function(prices) {
  return(rep(1/ncol(prices), ncol(prices)))
}


test_that("performance measures", {
  res <- portfolioBacktest(portfolio_fun_uniform, prices[[1]],
                           shortselling = FALSE, leverage = 1,
                           T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

  expect_that(abs(res$performance["sharpe ratio"] - 0.64357740) < 1e-8, is_true())
  expect_that(abs(res$performance["max drawdown"] - 0.08656783) < 1e-8, is_true())
  expect_that(abs(res$performance["expected return"] - 0.08220608) < 1e-8, is_true())
  expect_that(abs(res$performance["volatility"] - 0.12773302) < 1e-8, is_true())
  
  expect_that(sum(abs(res$returns - res_returns)) < 1e-8*nrow(res_returns), is_true())
})

