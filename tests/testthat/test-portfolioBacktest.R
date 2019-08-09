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



test_that("backtest results coincide with PerformanceAnalytics and base R", {
  prices <- dataset10[[1]]$adjusted[1:100, 1:2]
  colnames(prices) <- c("stock1", "stock2")
  X_lin <- (prices/lag(prices) - 1)[-1]
  N <- ncol(X_lin)  # number of stocks
  T <- nrow(X_lin)  # number of days
  
  #
  # daily rebalancing of EWP
  #
  bt <- portfolioBacktest(portfolios, 
                          dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                          T_rolling_window = 1,
                          optimize_every = 1, 
                          rebalance_every = 1,
                          return_portfolio = TRUE, 
                          return_returns = TRUE)
  ret_portfolioBacktest <- bt$Uniform$`dataset 1`$return
  wealth_portfolioBacktest <- bt$Uniform$`dataset 1`$cumPnL
  
  # compare with base R
  w_EWP <- rep(1/N, N)
  ret_direct <- xts(X_lin %*% w_EWP, index(X_lin))
  wealth_direct <- xts(c(1, cumprod(1 + ret_direct)), index(prices))  # compounded (initial budget of 1$)
  expect_equal(ret_portfolioBacktest, ret_direct, check.attributes = FALSE)
  expect_equal(wealth_portfolioBacktest, wealth_direct, check.attributes = FALSE)
  
  # compare with PerformanceAnalytics
  PerfAnal <- PerformanceAnalytics::Return.portfolio(X_lin, weights = w_EWP, rebalance_on = "days", verbose = TRUE)
  expect_equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
  expect_equal(bt$Uniform$`dataset 1`$w_bop, PerfAnal$BOP.Weight, check.attributes = FALSE)
  
  
  #
  # EWP rebalanced every 20 days
  # 
  bt <- portfolioBacktest(portfolios, 
                          dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                          T_rolling_window = 20,
                          optimize_every = 20, 
                          rebalance_every = 20,
                          return_portfolio = TRUE, 
                          return_returns = TRUE)
  ret_portfolioBacktest <- bt$Uniform$`dataset 1`$return
  #head(bt$Uniform$`dataset 1`$w_designed)
  #head(bt$Uniform$`dataset 1`$w_bop)
  
  # compare with PerformanceAnalytics
  PerfAnal <- PerformanceAnalytics::Return.portfolio(X_lin, weights = bt$Uniform$`dataset 1`$w_designed, verbose = TRUE)
  expect_equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
  expect_equal(bt$Uniform$`dataset 1`$w_bop, PerfAnal$BOP.Weight, check.attributes = FALSE)  
  

  #
  # GMVP rebalanced every 20 days
  #   
  ret_portfolioBacktest <- bt$GMVP$`dataset 1`$return
  
  # compare with PerformanceAnalytics
  expect_warning(PerfAnal <- PerformanceAnalytics::Return.portfolio(X_lin, weights = bt$GMVP$`dataset 1`$w_designed, verbose = TRUE), 
  "The weights for one or more periods do not sum up to 1: assuming a return of 0 for the residual weights")
  expect_equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
  expect_equal(bt$GMVP$`dataset 1`$w_bop, PerfAnal$BOP.Weight[, 1:2], check.attributes = FALSE)
  cash <- 1 - rowSums(bt$GMVP$`dataset 1`$w_bop)
  expect_equal(cash, as.vector(PerfAnal$BOP.Weight[, 3]), check.attributes = FALSE)
  
  
  #
  # Check "next day" execution vs "same day" execution
  #
  bt_next_day <- portfolioBacktest(portfolios, 
                          dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                          T_rolling_window = 20,
                          optimize_every = 20, 
                          rebalance_every = 20,
                          execution = "next day",
                          return_portfolio = TRUE, 
                          return_returns = TRUE)
  expect_equal(bt$Uniform$`dataset 1`$w_designed, bt_next_day$Uniform$`dataset 1`$w_designed)
  w_designed_lagged <- prices
  w_designed_lagged[] <- NA
  w_designed_lagged[index(bt$Uniform$`dataset 1`$w_designed), ] <- bt$Uniform$`dataset 1`$w_designed
  w_designed_lagged <- lag.xts(w_designed_lagged)
  w_designed_lagged <- w_designed_lagged[!is.na(w_designed_lagged[, 1])]
  
  # compare with PerformanceAnalytics
  PerfAnal_next_day <- PerformanceAnalytics::Return.portfolio(X_lin, weights = w_designed_lagged, verbose = TRUE)
  expect_equal(bt_next_day$Uniform$`dataset 1`$return, PerfAnal_next_day$returns, check.attributes = FALSE)
  expect_equal(bt_next_day$Uniform$`dataset 1`$w_bop, PerfAnal_next_day$BOP.Weight, check.attributes = FALSE)
})
  


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

# test_that("portfolioBacktest under parallel mode", {
#   bt_paral_portfolios <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_portfolios = 2,
#                                            shortselling = TRUE, leverage = Inf, 
#                                            return_portfolio = TRUE, return_returns = TRUE, 
#                                            benchmark = c("uniform", "index"),
#                                            T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
#   
#   
#   bt_paral_datasets <- portfolioBacktest(portfolios, dataset_list = dataset10, paral_datasets = 5,
#                                          shortselling = TRUE, leverage = Inf, 
#                                          return_portfolio = TRUE, return_returns = TRUE, 
#                                          benchmark = c("uniform", "index"),
#                                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
# 
#   load("bt_table_check.RData")
#   expect_equal(backtestTable(bt_paral_portfolios)[1:8], bt_table_check) 
#   expect_equal(backtestTable(bt_paral_datasets)[1:8], bt_table_check) 
# })

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

