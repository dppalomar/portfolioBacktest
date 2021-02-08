portfolio_fun <- function(dataset, ...) {
  prices <- dataset$adjusted
  return(rep(1/ncol(prices), ncol(prices)))
}