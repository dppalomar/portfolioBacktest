#' Backtesting Portfolio Design on a Rolling-Window Basis of Set of Prices
#'
#' Backtest a portfolio design contained in a function on a rolling-window basis of a set of prices.
#'
#' @param portfolio_fun function that takes as input an \code{xts} containing the stock prices and returns the portfolio weights.
#' @param prices \code{xts} containing the stock prices for the backtesting.
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
#' res <- backtestPortfolio(naivePortfolioDesign, prices[[1]])
#' print(res)
#'
#' @import xts
#' @export
backtestPortfolio <- function(portfolio_fun, prices, 
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

  
  
  
  
  return(list())
}
