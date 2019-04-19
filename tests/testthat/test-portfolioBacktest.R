context("Backtest results")

data(dataset)  # data in the package
#dataset_check <- to.monthly(dataset[[1]]$adjusted)$`dataset[[1]]$adjusted.Close`
#save(dataset_check, file = "dataset_check.RData")
load("dataset_check.RData")

test_that("the dataset used is the same", {
  expect_equal(dataset_check, to.monthly(dataset[[1]]$adjusted)$`dataset[[1]]$adjusted.Close`)
})



portfolio_fun_uniform <- function(dataset, prices = dataset$adjusted) {
  return(rep(1/ncol(prices), ncol(prices)))
}


test_that("backtest results and performance measures coincide with the precomputed ones", {
  bt <- portfolioBacktest(portfolio_fun_uniform, dataset[1],
                          shortselling = FALSE, leverage = 1,
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  #bt_check <- bt
  #save(bt_check, file = "bt_check.RData")
  load("bt_check.RData")
  expect_equal(bt$fun1$`dataset 1`[-2], bt_check$fun1$`dataset 1`[-2])  # compare except cpu time
  
  res_summary_median <- backtestSummary(bt, summary_fun = median)
  #res_summary_median_check <- res_summary_median
  #save(res_summary_median_check, file = "res_summary_median_check.RData")
  load("res_summary_median_check.RData")
  
  expect_equal(res_summary_median[-3], res_summary_median_check[-3])  # compare except cpu time
})


# # other test functions:
# expect_equal(cond$cov_y_diag,  diag(cond$cov_y))
# expect_error(riskParityPortfolio(Sigma, w_ub = rep(0.01, N)))
# expect_silent(riskParityPortfolio(Sigma, w_lb = 0.05, w_ub = 0.3))
# expect_warning(riskParityPortfolio(Sigma, w0 = rep(1, N)))
# expect_that(all.equal(rpp_mu, rpp, tolerance = 1e-4), is_true())
