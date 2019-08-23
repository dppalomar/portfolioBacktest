context("Checking package error control")

library(xts)
data(dataset10)
my_dataset <- dataset10[[1]]
names(my_dataset) <- c("open", "index")



test_that("Error control test for \"stockDataDownload\"", {
  
  sink(file = tempfile())
  expect_error(stockDataDownload("NOT_SYMBOL"), 
               "Arguments from and to have to be passed.")

  expect_error(stockDataDownload("NOT_SYMBOL", from = "1970-01-01", to = "1970-01-31", local_file_path = NULL), 
               "Failed to download data from any stock.")
  sink()
})

test_that("Error control test for \"stockDataResample\"", {
  
  X_wrong_index <- my_dataset
  index(X_wrong_index$index) <- index(X_wrong_index$index) + 1
  expect_error(stockDataResample(X_wrong_index), "The date indexes of \"X\" do not match.")
  
  X_non_mono <- my_dataset
  X_non_mono$open[2, ] <- NA
  expect_error(stockDataResample(X_non_mono), "\"X\" does not satisfy monotone missing-data pattern.")
  
  expect_error(stockDataResample(my_dataset, T = 1e10,), "\"T_sample\" can not be greater than the date length of \"X\".")
  
})

test_that("Error control test for \"portfolioBacktest\"", {
  
  expect_error(portfolioBacktest(paral_portfolios = -1), "Parallel number must be a positive interger.")
  
  expect_error(portfolioBacktest(paral_datasets = -1), "Parallel number must be a positive interger.")
  
  expect_error(portfolioBacktest(), "The \"folder_path\" and \"portfolio_fun_list\" cannot be both NULL.")
  
  expect_error(portfolioBacktest(list("fun1" = 1, 2)), "Each element of \"portfolio_funs\" must has a unique name.")
  
  expect_error(portfolioBacktest(list("fun1" = 1, "fun1" = 2)), "\"portfolio_funs\" contains repeated names.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), my_dataset),  "Fail to find price data with name \"adjusted\" in given dataset_list.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), list(list("adjusted" = 1))),  "prices have to be xts.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10, T_rolling_window = 1e10),  "T is not large enough for the given sliding window length.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10, optimize_every = 3, rebalance_every = 2),  "The reoptimization period has to be a multiple of the rebalancing period.")
  
  X_wNA <- dataset10
  X_wNA$`dataset 1`$adjusted[1, ] <- NA
  expect_error(portfolioBacktest(list("fun1" = 1), X_wNA),  "prices contain NAs.")
  
  expect_error(portfolioBacktest(list("fun1" = 1), dataset10),  "portfolio_fun is not a function.")
  
})


# define uniform portfolio
uniform_portfolio_fun <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}

bt <- portfolioBacktest(uniform_portfolio_fun, dataset10, benchmark = c("uniform", "index"))

test_that("Error control test for \"backtestSelector\"", {
  
  expect_error(backtestSelector(measures = "NOT_MEASURE"), "\"measures\" contains invalid element.")
  
  expect_error(backtestSelector(measures = integer(0)), "\"measures\" must have length > 1.")
  
  expect_error(backtestSelector(), "must select a portfolio.")
  
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
  expect_error(genRandomFuns(portfolio_fun = uniform_portfolio_fun,
                             params_grid = list(lookback = c(100, 120, 140, 160),
                                                delay = c(0, 5, 10, 15, 20),
                                                regularize = c(FALSE, TRUE))),
               "Number of functions to be generated \"N_funs\" has to be specified")
  
  expect_warning(tmp <- genRandomFuns(portfolio_fun = uniform_portfolio_fun,
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
  portfolio_list <- genRandomFuns(portfolio_fun = uniform_portfolio_fun,
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


