library(portfolioBacktest)
data(dataset10)


test_portfolio <- function(dataset, ...) {
  N <- ncol(dataset$adjusted)
  
  print(var)
  w <- rep(var, N)
  var <<- 1/N

  return(w)
}


var <- 0
bt <- portfolioBacktest(list("test" = test_portfolio), 
                        dataset_list = dataset10[1:2],
                        lookback = 100, optimize_every = 200,
                        paral_datasets = 2)
                        #paral_portfolios = 2)   #  <-- doesn't work with this

bt$test$`dataset 1`$w_optimized[, 1:2]
bt$test$`dataset 2`$w_optimized[, 1:2]

backtestChartCumReturn(bt)

