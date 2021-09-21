context("Checking package error control")

#library(testthat)
#library(portfolioBacktest)
library(xts)
data(dataset10)
my_dataset <- dataset10[[1]]


test_that("Error control test for \"stockDataDownload\"", {
  sink(file = tempfile())
  expect_error(stockDataDownload("NOT_SYMBOL"), 
               "Arguments from and to have to be passed.")
  expect_error(stockDataDownload("NOT_SYMBOL", from = "1970-01-01", to = "1970-01-31", local_file_path = NULL), 
               "Failed to download data from any stock.")
  sink()
})


test_that("Error control test for \"financialDataResample\"", {
  X_wrong_index <- my_dataset
  index(X_wrong_index$index) <- index(X_wrong_index$index) + 1
  expect_error(financialDataResample(X_wrong_index), "The date indices of \"X\" do not match.")
  
  X_non_mono <- my_dataset
  X_non_mono$adjusted[2, ] <- NA
  #expect_error(financialDataResample(X_non_mono), "\"X\" does not satisfy monotone missing-data pattern.")
  expect_error(financialDataResample(X_non_mono), "Time period without any stock without NAs!")
  expect_error(financialDataResample(my_dataset, T = 1e10), "\"T_sample\" cannot be greater than the date length of \"X\".")
})


test_that("Error control test for \"portfolioBacktest\"", {
  
  expect_error(portfolioBacktest(paral_portfolios = -1), "Parallel number must be a positive interger.")
  
  expect_error(portfolioBacktest(paral_datasets = -1), "Parallel number must be a positive interger.")
  
  expect_error(portfolioBacktest(), "The \"folder_path\" and \"portfolio_fun_list\" cannot be both NULL.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), my_dataset),  "Each element of \"dataset_list\" must be a list of xts objects. Try to surround your passed \"dataset_list\" with list().")
  
  expect_error(portfolioBacktest(list("fun1" = 1, 2), dataset10), "Each element of \"portfolio_funs\" must has a unique name.")
  
  expect_error(portfolioBacktest(list("fun1" = 1, "fun1" = 2), dataset10), "\"portfolio_funs\" contains repeated names.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10, price_name = "lala"),
               "Price data xts element \"lala\" does not exist in dataset_list.")
  
  names(my_dataset) <- c("open", "index")
  expect_error(portfolioBacktest(list("fun1" = 1), list(my_dataset)), 
               "Price data xts element \"adjusted\" does not exist in dataset_list.")

  expect_error(portfolioBacktest(list("fun1" = 1), list(list("adjusted" = 1))),  "prices have to be xts.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10, lookback = 1e10),  "T is not large enough for the given lookback window length.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10, optimize_every = 3, rebalance_every = 2),  
               "The reoptimization period has to be a multiple of the rebalancing period.")
  
  
  X_wNA <- dataset10
  X_wNA$`dataset 1`$adjusted[1, ] <- NA
  expect_error(portfolioBacktest(list("fun1" = 1), X_wNA),  "prices contain NAs.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10),  "portfolio_fun is not a function.")
  
})



# test_that("Error control for index type of xts data", {
#   dataset_tmp <- dataset10
#   #tclass(dataset_tmp[[1]]$adjusted)
#   dataset_tmp[[1]]$adjusted <- convertIndex(dataset_tmp[[1]]$adjusted, "POSIXct")
#   #tclass(dataset_tmp[[1]]$adjusted)
#   
#   #expect_message(bt <- portfolioBacktest(ewp_fun, dataset_tmp[1]),
#   #               "Backtesting 1 portfolios over 1 datasets (periodicity = daily data)")
#   
#   # dataset_tmp[[2]]$adjusted <- to.monthly(dataset_tmp[[2]]$adjusted)
#   # #tclass(dataset_tmp[[2]]$adjusted)
#   # expect_error(portfolioBacktest(ewp_fun, dataset_tmp[2], lookback = 10),
#   #              "This function only accepts daily data")
# })



bt <- portfolioBacktest(portfolioBacktest:::uniform_portfolio_fun, dataset10, 
                        benchmarks = c("uniform", "index"))

test_that("Error control test for \"backtestSelector\"", {
  
  expect_error(backtestSelector(bt, portfolio_index = 1, measures = "NOT_MEASURE"), "\"measures\" contains invalid element.")
  
  expect_error(backtestSelector(bt, portfolio_index = 1, measures = integer(0)), "\"measures\" must have length > 1.")
  
  expect_error(backtestSelector(bt), "must select a portfolio.")
  
  expect_error(backtestSelector(bt, portfolio_name = c("FIRST", "SECOND")), "Only one portfolio can be selected.")
})


test_that("Error control test for \"backtestTable\"", {
  expect_error(backtestTable(bt, measures = "NOT_MEASURE"), "\"measures\" contains invalid element.")
})


test_that("Error control test for \"backtestLeaderboard\"", {
  
  expect_error(backtestLeaderboard(bt, 1), "Argument \"weights\" must be a list.")
  
  expect_error(backtestLeaderboard(bt, list(-1)), "All weights must be non-negative.")
  
  expect_error(backtestLeaderboard(bt, as.list(rep(0, 2))), "Cannot set all weights be zero.")
  
  expect_error(backtestLeaderboard(bt, list("NOT_NAME" = 1)), "Contain invalid elements in \"weights\".")
  
})



test_that("Error control test for \"genRandomFuns\"", {
  expect_error(genRandomFuns(portfolio_fun = portfolioBacktest:::uniform_portfolio_fun,
                             params_grid = list(lookback = c(100, 120, 140, 160),
                                                delay = c(0, 5, 10, 15, 20),
                                                regularize = c(FALSE, TRUE))),
               "Number of functions to be generated \"N_funs\" has to be specified")
  
  expect_warning(tmp <- genRandomFuns(portfolio_fun = portfolioBacktest:::uniform_portfolio_fun,
                                      params_grid = list(lookback = c(100, 120, 140, 160),
                                                         delay = c(0, 5, 10, 15, 20),
                                                         regularize = c(FALSE, TRUE)),
                                      N_funs = 100),
               "Too many functions requested for only 40 possible combinations: using instead N_funs = 40.")
  expect_equal(attr(tmp, "params_grid"), list(lookback = c(100, 120, 140, 160),
                                              delay = c(0, 5, 10, 15, 20),
                                              regularize = c(FALSE, TRUE)))
})



test_that("Error control test for \"plotPerformanceVsParams\"", {
  portfolio_list <- genRandomFuns(portfolio_fun = portfolioBacktest:::uniform_portfolio_fun,
                                  params_grid = list(lookback = c(100, 120, 140, 160),
                                                     delay = c(0, 5, 10, 15, 20),
                                                     regularize = c(FALSE, TRUE)),
                                  N_funs = 5)  
  bt <- portfolioBacktest(portfolio_list, dataset10[1:2])
  
  expect_error(p <- plotPerformanceVsParams(bt, params_subset = list(lookback3 = TRUE)),
               "Argument \"params_subset\" contains parameters not contained in the backtest.")
  
  expect_error(p <- plotPerformanceVsParams(bt, params_subset = list(lookback = 99)),
               "Element lookback of argument \"params_subset\" is not contained in the backtest.")
  
  attr(bt[[1]], "params") <- NULL
  expect_error(p <- plotPerformanceVsParams(bt),
               "Backtest does not contain the attribute \"params\"!")
})


