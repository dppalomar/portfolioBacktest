#' Backtesting Portfolio Design on a Rolling-Window Basis of Set of Prices
#'
#' Backtest a portfolio design contained in a function on a rolling-window basis of a set of prices.
#'
#' @param portfolio_fun function that takes as input an \code{xts} containing the stock prices and returns the portfolio weights.
#' @param prices \code{xts} containing the stock prices for the backtesting.
#' @param shortselling whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage).
#' @param T_sliding_window length of the sliding window.
#' @param freq_optim how often the portfolio is to be reoptimized.
#' @param freq_rebalance how often the portfolio is to be rebalanded.
#' @return A list containing the performance in the following elements:
#' \item{\code{TBD}  }{m-by-m matrix, columns corresponding to eigenvectors.}
#' \item{\code{TBD}  }{m-by-1 vector corresponding to eigenvalues.}
#' @author Daniel P. Palomar and Rui Zhou
#' @examples
#' library(backtestPortfolio)
#' library(xts)
#'
#' # load data
#' data(prices)
#'
#' # define portfolio function
#' portfolio_fun <- function(prices) {
#'   X <- diff(log(prices))[-1]  # compute log returns
#'   Sigma <- cov(X)  # compute SCM
#'   # design GMVP
#'   w <- solve(Sigma, rep(1, nrow(Sigma)))
#'   w <- w/sum(w)
#'   return(w)
#' }
#' 
#' # perform backtesting
#' res <- backtestPortfolio(portfolio_fun, prices[[1]])
#' print(res)
#'
#' @import xts
#' @export
backtestPortfolio <- function(portfolio_fun, prices,
                              shortselling = FALSE, leverage = 1,
                              T_sliding_window = 6*21, freq_optim = 5, freq_rebalance = freq_optim) {
  ######## error control  #########
  if (is.list(prices)) stop("prices have to be xts, not a list, make sure you index the list with double brackets [[.]]")
  if (!is.xts(prices)) stop("prices have to be xts")
  N <- ncol(prices)
  T <- nrow(prices)
  if (T_sliding_window >= T) stop("T is not large enough for the given sliding window length")
  if (freq_optim > freq_rebalance) stop("You cannot reoptimize more frequently that you rebalance")
  if (anyNA(prices)) stop("prices contain NAs")
  if (!is.function(portfolio_fun)) stop("portfolio_fun is not a function")
  #################################
  
  # indices
  #rebalancing_indices <- endpoints(prices, on = "weeks")[which(endpoints(prices, on = "weeks") >= T_sliding_window)]
  optim_indices <- seq(from = T_sliding_window, to = T, by = freq_optim)
  rebalancing_indices <- seq(from = T_sliding_window, to = T, by = freq_rebalance)
  
  # compute w
  w <- xts(matrix(NA, length(rebalancing_indices), N), order.by = index(prices)[rebalancing_indices])
  colnames(w) <- colnames(prices)
  for (i in 1:length(rebalancing_indices)) {
    idx_prices <- rebalancing_indices[i]
    prices_window <- prices[(idx_prices-T_sliding_window+1):idx_prices, ]
    w[i, ] <- do.call(portfolio_fun, list(prices_window))
    # make sure portfolio is feasible
    # Daniel: TBD
  }
  
  # compute returns of portfolio
  R_lin <- PerformanceAnalytics::CalculateReturns(prices)[-1]  #Daniel: fix later, no need to compute the initial T_sliding_window returns
  rets <- returnPortfolio(R = R_lin, weights = w)
  
  # compute cumulative wealth
  wealth_arith_BnH_trn <- 1 + cumsum(rets)  # initial budget of 1$
  wealth_geom_BnH_trn <- cumprod(1 + rets)  # initial budget of 1$
  
  # compute various performance measures
    # Daniel: think of turnover and ROI
    # Rui: 
    #
    # I would return a list with the following elements:
    # $returns: this is a one-column xts with the daily returns
    # $cumPnL: this is a one-column xts with the daily cumPnL
    # $performance: this could be a named vector with each element containing stuff like
    #               Sharpe ratio, max drawdown, expected return, volatility, etc. 
    #               You can use PerformanceAnalytics to compute those better, so that the code is short and clean.
    # $error: this could be a number indicating the type of error: 0 is no error, 1 is whatever, 2 is whatever
    # $message: this could contain some string message if there was some error (to help the user understand the error) or NULL otherwise
    #
  return(list("returns" = rets,
              "cumPnL" = wealth_geom_BnH_trn))
}


#' @export
multipleBacktestPortfolio <- function(portfolio_fun, prices,
                              shortselling = FALSE, leverage = 1,
                              T_sliding_window = 6*21, freq_optim = 5, freq_rebalance = freq_optim) {
  # Rui: this one receives in prices a list of xts and loops over them calling backtestPortfolio
  # It should returs a list containing something similar to the return of backtestPortfolio:
  # $returns and $cumPnL now will be a matrix xts with each column corresponding to each call to backtestPortfolio
  # $performance now will be a matrix where each column will contain each vector returned by each call to backtestPortfolio
  # $ error now will be a vector
  # $ message now will be a vector
  
  # BTW, eventually I want to merge multipleBacktestPortfolio and backtestPortfolio into just one function called backtestPortfolio
  # but let's do that later. I will have time this weekend and Monday.
}


#
# Computes the returns of a portfolio of several assets (ignoring transaction costs):
#   R: is an xts with the individual asset linear returns (not log returns)
#   weights: is an xts with the normalized dollar allocation (wrt NAV, typically with sum=1) where
#            - each row represents a rebalancing date (with portfolio computed with info up to and including that day)
#            - dates with no rows means no rebalancing (note that the portfolio may then violate some margin constraints...)
#
#' @import xts
# Daniel: think carefully of the effect of shorselling and leverage in this function
returnPortfolio <- function(R, weights, execution = c("same day", "next day"), name = "portfolio.returns") {
  ######## error control  #########
  if (!is.xts(R) || !is.xts(weights)) stop("This function only accepts xts")
  if (attr(index(R), "class") != "Date") stop("This function only accepts daily data")
  if (!all(index(weights) %in% index(R))) stop("Weight dates do not appear in the returns")
  if (ncol(R) != ncol(weights)) stop("Number of weights does not match the number of assets in the returns")
  R[is.na(R)] <- 0
  #################################
  
  # fill in w with NA to match the dates of R and lag appropriately
  w <- R; w[] <- NA
  w[index(weights), ] <- weights
  switch(match.arg(execution),
         "same day" = { w <- lag(w) },
         "next day" = { w <- lag(w, 2) },
         stop("Execution method unknown")
  )
  rebalance_indices <- which(!is.na(w[, 1]))
  # loop
  ret <- xts(rep(NA, nrow(R)), order.by = index(R))
  colnames(ret) <- name
  for (i in rebalance_indices[1]:nrow(R)) {
    if (i %in% rebalance_indices) {
      cash <- 1 - sum(w[i, ])       # normalized cash wrt NAV
      ret[i] <- sum(R[i, ]*w[i, ])  # recall w is normalized wrt NAV
      w_eop <- (1 + R[i, ])*w[i, ]  # new w but it is still normalized wrt previous NAV which is not the correct normalization
    }
    else {
      cash <- 1 - sum(w_eop)       # normalized cash wrt NAV
      ret[i] <- sum(R[i, ]*w_eop)  # recall w is normalized wrt NAV
      w_eop <- (1 + R[i, ])*w_eop  # new w but it is still normalized wrt previous NAV which is not the correct normalization
    } 
    NAV_change <- cash + sum(w_eop)       # NAV(t+1)/NAV(t)
    w_eop <- as.vector(w_eop/NAV_change)  # now w_eop is normalized wrt the current NAV
  }
  return(ret[rebalance_indices[1]:nrow(ret), ])
}

