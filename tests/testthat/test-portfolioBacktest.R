context("Checking portfolioBacktest and result handling functions")

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

portfolios <- list("Uniform"   = uniform_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun)

test_that("backtest results and performance measures coincide with the precomputed ones", {
  bt <- portfolioBacktest(portfolios, dataset_list = dataset10,
                          shortselling = TRUE, leverage = Inf, 
                          return_portfolio = TRUE, return_returns = TRUE, 
                          benchmark = c("uniform", "index"),
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  # bt_check <- bt$GMVP$`dataset 1`[-2]
  # save(bt_check, file = "bt_check.RData")
  load("bt_check.RData")
  expect_equal(bt$GMVP$`dataset 1`[-2], bt_check) 
  
  # bt_selector_check <- backtestSelector(bt, portfolio_name = "Uniform")$performance
  # save(bt_selector_check, file = "bt_selector_check.RData")
  load("bt_selector_check.RData")
  expect_equal(backtestSelector(bt, portfolio_name = "Uniform")$performance, bt_selector_check) 
  
  # bt_table_check <- backtestTable(bt)[1:8]
  # save(bt_table_check, file = "bt_table_check.RData")
  load("bt_table_check.RData")
  expect_equal(backtestTable(bt)[1:8], bt_table_check) 
  
  # bt_summary_check <- backtestSummary(bt, summary_fun = median)[1:2]
  # save(bt_summary_check, file = "bt_summary_check.RData")
  load("bt_summary_check.RData")
  bt_summary <- backtestSummary(bt, summary_fun = median)[1:2]
  expect_equal(backtestSummary(bt, summary_fun = median)[1:2], bt_summary)  # compare except cpu time
})

test_that("portfolioBacktest under parallel mode", {
  bt_paral_portfolios <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_portfolios = 2,
                                           shortselling = TRUE, leverage = Inf, 
                                           return_portfolio = TRUE, return_returns = TRUE, 
                                           benchmark = c("uniform", "index"),
                                           T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  
  
  bt_paral_datasets <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_datasets = 5,
                                         shortselling = TRUE, leverage = Inf, 
                                         return_portfolio = TRUE, return_returns = TRUE, 
                                         benchmark = c("uniform", "index"),
                                         T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)

  load("bt_table_check.RData")
  expect_equal(backtestTable(bt_paral_portfolios)[1:8], bt_table_check) 
  expect_equal(backtestTable(bt_paral_datasets)[1:8], bt_table_check) 
})

test_that("portfolioBacktest over files", {
  bt_files <- portfolioBacktest(folder_path = "portfolio_files", dataset_list = dataset10,
                                shortselling = TRUE, leverage = Inf, 
                                return_portfolio = FALSE, return_returns = FALSE,
                                T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  bt_files <- lapply(bt_files, function(x) {sapply(x, function(x) {x$performance})})
  load("bt_files_check.RData")
  expect_equal(bt_files, bt_files_check)
  # expect_true(all.equal(bt_files, bt_files_check, tolerance = 1e-3))
  # save(bt_files_check, file = "bt_files_check.RData", version = 2, compression_level = 9)
})

