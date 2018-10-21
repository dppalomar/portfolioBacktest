data(prices)
load("res_returns.RData")
load("prices1.RData")

portfolio_fun_uniform <- function(prices) {
  return(rep(1/ncol(prices), ncol(prices)))
}

test_that("dataset prices1", {
  expect_equal(prices1, prices[[1]])
})


test_that("performance measures", {
  res <- portfolioBacktest(portfolio_fun_uniform, prices1,
                           shortselling = FALSE, leverage = 1,
                           T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

  expect_that(abs(res$performance["sharpe ratio"] - 1.46631697) < 1e-8, is_true())
  expect_that(abs(res$performance["max drawdown"] - 0.08664096) < 1e-8, is_true())
  expect_that(abs(res$performance["expected return"] - 0.18915374) < 1e-8, is_true())
  expect_that(abs(res$performance["volatility"] - 0.12899922) < 1e-8, is_true())
  expect_that(abs(res$performance["ROT"] - 476.77241212) < 1e-8, is_true())
  
  expect_that(sum(abs(res$returns - res_returns)) < 1e-8*nrow(res_returns), is_true())
})
