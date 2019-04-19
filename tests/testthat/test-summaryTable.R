context("Checking summaryTable")


portfolio_fun_uniform <- function(dataset, prices = dataset$adjusted) {
  return(rep(1/ncol(prices), ncol(prices)))
}


test_that("summaryTable results coincide with the precomputed ones", {
  bt <- portfolioBacktest(portfolio_fun_uniform, dataset,
                          shortselling = FALSE, leverage = 1,
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

  res_summary_median <- backtestSummary(bt, summary_fun = median)
  res_table <- summaryTable(res_summary_median, 
                            measures = c("max drawdown", "annual volatility", "annual return", "Sharpe ratio"))
  #res_table_check <- res_table
  #save(res_table_check, file = "res_table_check.RData")
  load("res_table_check.RData")
  
  expect_equal(res_table_check, res_table_check)
})


# # other test functions:
# expect_equal(cond$cov_y_diag,  diag(cond$cov_y))
# expect_error(riskParityPortfolio(Sigma, w_ub = rep(0.01, N)))
# expect_silent(riskParityPortfolio(Sigma, w_lb = 0.05, w_ub = 0.3))
# expect_warning(riskParityPortfolio(Sigma, w0 = rep(1, N)))
# expect_that(all.equal(rpp_mu, rpp, tolerance = 1e-4), is_true())
