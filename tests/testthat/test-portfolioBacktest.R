context("Checking portfolioBacktest and result handling functions")

#library(testthat)
#library(portfolioBacktest)
library(xts)
data(dataset10)

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
  wealth_portfolioBacktest <- bt$Uniform$`dataset 1`$wealth
  
  # compare with base R
  w_EWP <- rep(1/N, N)
  ret_direct <- xts(X_lin %*% w_EWP, index(X_lin))
  wealth_direct <- xts(c(1, cumprod(1 + ret_direct)), index(prices))  # compounded (initial budget of 1$)
  expect_equivalent(ret_portfolioBacktest, ret_direct)
  expect_equivalent(wealth_portfolioBacktest, wealth_direct)
  
  # compare with PerformanceAnalytics
  PerfAnal <- PerformanceAnalytics::Return.portfolio(X_lin, weights = w_EWP, rebalance_on = "days", verbose = TRUE)
  expect_equivalent(ret_portfolioBacktest, PerfAnal$returns)
  expect_equivalent(bt$Uniform$`dataset 1`$w_bop, PerfAnal$BOP.Weight)
  
  
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
  expect_equivalent(ret_portfolioBacktest, PerfAnal$returns)
  expect_equivalent(bt$Uniform$`dataset 1`$w_bop, PerfAnal$BOP.Weight)  
  

  #
  # GMVP rebalanced every 20 days
  #   
  ret_portfolioBacktest <- bt$GMVP$`dataset 1`$return
  
  # compare with PerformanceAnalytics
  expect_warning(PerfAnal <- PerformanceAnalytics::Return.portfolio(X_lin, weights = bt$GMVP$`dataset 1`$w_designed, verbose = TRUE), 
                 "The weights for one or more periods do not sum up to 1: assuming a return of 0 for the residual weights")
  expect_equivalent(ret_portfolioBacktest, PerfAnal$returns)
  expect_equivalent(bt$GMVP$`dataset 1`$w_bop, PerfAnal$BOP.Weight[, 1:2])
  cash <- 1 - rowSums(bt$GMVP$`dataset 1`$w_bop)
  expect_equivalent(cash, as.vector(PerfAnal$BOP.Weight[, 3]))
  
  
  #
  # Check "next period" execution vs "same period" execution
  #
  bt_next_period <- portfolioBacktest(portfolios, 
                          dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                          T_rolling_window = 20,
                          optimize_every = 20, 
                          rebalance_every = 20,
                          execution = "next period",
                          return_portfolio = TRUE, 
                          return_returns = TRUE)
  expect_equivalent(bt$Uniform$`dataset 1`$w_designed, bt_next_period$Uniform$`dataset 1`$w_designed)
  w_designed_lagged <- prices
  w_designed_lagged[] <- NA
  w_designed_lagged[index(bt$Uniform$`dataset 1`$w_designed), ] <- bt$Uniform$`dataset 1`$w_designed
  w_designed_lagged <- lag.xts(w_designed_lagged)
  w_designed_lagged <- w_designed_lagged[!is.na(w_designed_lagged[, 1])]
  
  # compare with PerformanceAnalytics
  PerfAnal_next_day <- PerformanceAnalytics::Return.portfolio(X_lin, weights = w_designed_lagged, verbose = TRUE)
  expect_equivalent(bt_next_period$Uniform$`dataset 1`$return, PerfAnal_next_day$returns)
  expect_equivalent(bt_next_period$Uniform$`dataset 1`$w_bop, PerfAnal_next_day$BOP.Weight)
})
  


test_that("backtest results and performance measures coincide with the precomputed ones", {
  bt <- portfolioBacktest(portfolios, dataset_list = dataset10,
                          shortselling = TRUE, leverage = Inf, 
                          return_portfolio = TRUE, return_returns = TRUE, 
                          benchmark = c("uniform", "index"),
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  # bt_check <- bt$GMVP$`dataset 1`[-2]
  # save(bt_check, file = "bt_check.RData", version = 2)
  load("bt_check.RData")
  expect_equivalent(bt$GMVP$`dataset 1`[-2], bt_check)

  # bt_selector_check <- backtestSelector(bt, portfolio_name = "Uniform")$performance
  # save(bt_selector_check, file = "bt_selector_check.RData", version = 2)
  load("bt_selector_check.RData")
  expect_equivalent(backtestSelector(bt, portfolio_name = "Uniform")$performance, bt_selector_check)
  
  # bt_table_check <- backtestTable(bt)[1:8]
  # save(bt_table_check, file = "bt_table_check.RData", version = 2)
  load("bt_table_check.RData")
  expect_equivalent(backtestTable(bt)[1:8], bt_table_check) 
  
  # bt_summary_check <- head(backtestSummary(bt, summary_fun = median)[[1]], -2)
  # save(bt_summary_check, file = "bt_summary_check.RData", version = 2)
  load("bt_summary_check.RData")
  expect_equivalent(head(backtestSummary(bt, summary_fun = median)[[1]], -2), bt_summary_check)  # compare except cpu time
})


test_that("backtest results with bankruptcy work fine", {
  # force one stock to go bankrupt
  dataset10_bankruptcy <- dataset10[1]
  dataset10_bankruptcy$`dataset 1`$adjusted[, 1] <- 
    dataset10_bankruptcy$`dataset 1`$adjusted[, 1] - 0.08*(1:nrow(dataset10_bankruptcy$`dataset 1`$adjusted))
  #plot(dataset10_bankruptcy$`dataset 1`$adjusted[, 1])

  stock1_portfolio_fun <- function(dataset, prices = dataset$adjusted) {
    return(c(1, rep(0, ncol(prices)-1)))
  }
  
  bt <- portfolioBacktest(stock1_portfolio_fun, dataset_list = dataset10_bankruptcy,
                          shortselling = TRUE, leverage = Inf, 
                          return_portfolio = TRUE, return_returns = TRUE, 
                          benchmark = c("uniform", "index"),
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  first_date_trading <- index(bt$fun1$`dataset 1`$wealth)[1]
  stock_price_normalized <- dataset10_bankruptcy$`dataset 1`$adjusted[paste0(first_date_trading, "::"), 1]/as.numeric(dataset10_bankruptcy$`dataset 1`$adjusted[first_date_trading, 1])
  #plot(cbind(stock_price_normalized, bt$fun1$`dataset 1`$wealth))
  
  # check bankruptcy_dates
  expect_equivalent(index(stock_price_normalized[stock_price_normalized <= 0])[1],
                    index(bt$fun1$`dataset 1`$wealth[bt$fun1$`dataset 1`$wealth <= 0])[1])
  bankruptcy_date <- index(stock_price_normalized[stock_price_normalized <= 0])[1]
  
  # check whole time series until bankruptcy
  expect_equivalent(stock_price_normalized[paste0("::", bankruptcy_date)],
                    bt$fun1$`dataset 1`$wealth[paste0("::", bankruptcy_date)])
})



test_that("transaction cost works properly", {
  bt <- portfolioBacktest(portfolios[1], dataset_list = dataset10[1],
                          shortselling = TRUE, leverage = Inf,
                          return_portfolio = TRUE, return_returns = TRUE,
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  
  bt2 <- portfolioBacktest(portfolios[1], dataset_list = dataset10[1],
                          shortselling = TRUE, leverage = Inf,
                          cost = list(buy = 0, sell = 0),
                          return_portfolio = TRUE, return_returns = TRUE,
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  expect_equivalent(bt$Uniform$`dataset 1`[-2], bt2$Uniform$`dataset 1`[-2])

  bt_tc <- portfolioBacktest(portfolios[1], dataset_list = dataset10[1],
                           shortselling = TRUE, leverage = Inf,
                           cost = list(buy = 1e-4, sell = 0),
                           return_portfolio = TRUE, return_returns = TRUE,
                           T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  
  expect_equivalent(bt$Uniform$`dataset 1`[c("error", "error_message", "w_designed")], 
                    bt_tc$Uniform$`dataset 1`[c("error", "error_message", "w_designed")])
  expect_equivalent(bt$Uniform$`dataset 1`$w_bop,
                    bt_tc$Uniform$`dataset 1`$w_bop, tolerance = 1e-5)
  expect_equivalent(bt$Uniform$`dataset 1`$return,
                    bt_tc$Uniform$`dataset 1`$return, tolerance = 2e-4)
  #plot(cbind(bt$Uniform$`dataset 1`$return, bt_tc$Uniform$`dataset 1`$return))
  expect_equivalent(bt$Uniform$`dataset 1`$wealth,
                    bt_tc$Uniform$`dataset 1`$wealth, tolerance = 2e-4)
  #plot(cbind(bt$Uniform$`dataset 1`$wealth, bt_tc$Uniform$`dataset 1`$wealth))
})


test_that("cash is properly accounted in backtest results", {
  # create stock with bankruptcy
  dataset10_bankruptcy <- dataset10[1]
  T <- nrow(dataset10_bankruptcy$`dataset 1`$adjusted)
  dataset10_bankruptcy$`dataset 1`$adjusted[400:T, 1] <- 1e-6
  #plot(dataset10_bankruptcy$`dataset 1`$adjusted[, 1])

  # first all invested in a stock that goes bankrupt
  stock1_portfolio_fun <- function(dataset, prices = dataset$adjusted) {
    return(c(1, rep(0, ncol(prices)-1)))
  }
  bt <- portfolioBacktest(stock1_portfolio_fun, dataset_list = dataset10_bankruptcy,
                          shortselling = TRUE, leverage = Inf, 
                          return_portfolio = TRUE, return_returns = TRUE, 
                          benchmark = c("uniform", "index"),
                          T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  first_date_trading <- index(bt$fun1$`dataset 1`$wealth)[1]
  stock_price_normalized <- dataset10_bankruptcy$`dataset 1`$adjusted[paste0(first_date_trading, "::"), 1]/as.numeric(dataset10_bankruptcy$`dataset 1`$adjusted[first_date_trading, 1])
  #plot(cbind(stock_price_normalized, bt$fun1$`dataset 1`$wealth), lwd = c(2, 4))
  expect_equivalent(stock_price_normalized, bt$fun1$`dataset 1`$wealth)

    
  # second, all in cash
  stock1_portfolio_fun <- function(dataset, prices = dataset$adjusted) {
    return(c(0, rep(0, ncol(prices)-1)))
  }
  bt <- suppressWarnings(expr = portfolioBacktest(stock1_portfolio_fun, dataset_list = dataset10_bankruptcy,
                                                  shortselling = TRUE, leverage = Inf, 
                                                  return_portfolio = TRUE, return_returns = TRUE, 
                                                  benchmark = c("uniform", "index"),
                                                  T_rolling_window = 252, optimize_every = 20, rebalance_every = 5))
  #plot(cbind(stock_price_normalized, bt$fun1$`dataset 1`$wealth), lwd = c(2, 4))
  expect_equivalent(sum(abs(bt$fun1$`dataset 1`$wealth - 1)), 0)
    
  # # third, partially invested
  # stock1_portfolio_fun <- function(dataset, prices = dataset$adjusted) {
  #   return(c(0.5, rep(0, ncol(prices)-1)))
  # }
  # bt <- portfolioBacktest(stock1_portfolio_fun, dataset_list = dataset10_bankruptcy,
  #                         shortselling = TRUE, leverage = Inf, 
  #                         return_portfolio = TRUE, return_returns = TRUE, 
  #                         benchmark = c("uniform", "index"),
  #                         T_rolling_window = 252, optimize_every = 20, rebalance_every = 5)
  # plot(cbind(stock_price_normalized, bt$fun1$`dataset 1`$wealth), lwd = c(2, 4))
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
  expect_equivalent(bt_files, bt_files_check)
  # bt_files_check <- bt_files
  # save(bt_files_check, file = "bt_files_check.RData", version = 2, compression_level = 9)
})

