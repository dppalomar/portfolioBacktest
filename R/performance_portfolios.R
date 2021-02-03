
portfolioPerformance <- function(rets, ROT_bps, bars_per_year) {
  # create empty return object
  performance <- c("Sharpe ratio" = NA, "max drawdown" = NA, "annual return" = NA, "annual volatility" = NA, 
                   "Sterling ratio" = NA, "Omega ratio" = NA, "ROT (bps)" = NA, "VaR (0.95)" = NA, "CVaR (0.95)" = NA)
  attr(performance, "judge") <- c(1, -1, 1, -1, 1, 1, 1, -1, -1)
  
  if (!anyNA(rets)) {
    fraction_in <- sum(abs(rets) > 1e-8)/nrow(rets)
    rets <- rets[abs(rets) > 1e-8]  # remove data where return is zero
    if (nrow(rets) == 0)
      performance[] <- c(NA, 0, 0, 0, NA, NA, NA, 0, 0)
    else {
      performance["Sharpe ratio"]      <- PerformanceAnalytics::SharpeRatio.annualized(rets, scale = bars_per_year) * sqrt(fraction_in)
      performance["max drawdown"]      <- PerformanceAnalytics::maxDrawdown(rets)
      performance["annual return"]     <- PerformanceAnalytics::Return.annualized(rets, scale = bars_per_year) * fraction_in        # prod(1 + rets)^(252/nrow(rets)) - 1 or mean(rets) * 252
      performance["annual volatility"] <- PerformanceAnalytics::StdDev.annualized(rets, scale = bars_per_year) * sqrt(fraction_in)  # sqrt(252) * sd(rets, na.rm = TRUE)
      performance["Sterling ratio"]    <- performance["annual return"] / performance["max drawdown"]
      performance["Omega ratio"]       <- PerformanceAnalytics::Omega(rets)
      performance["ROT (bps)"]         <- ROT_bps
      performance["VaR (0.95)"]        <- PerformanceAnalytics::VaR(rets, 0.95, method = "historical", invert = FALSE)
      performance["CVaR (0.95)"]       <- PerformanceAnalytics::CVaR(rets, 0.95, method = "historical", invert = FALSE)
    }
  }
  return(performance)
}



#' @title Add a new performance measure to backtests
#'
#' @param bt Backtest results as produced by the function \code{\link{portfolioBacktest}}.
#' @param name String with name of new performance measure.
#' @param fun Function to compute new performance measure from any element returned by 
#'            \code{\link{portfolioBacktest}}, e.g., \code{return}, \code{wealth}, and \code{w_bop}.
#' @param desired_direction Number indicating whether the new measure is desired to be larger (1),
#'                          which is the default, or smaller (-1).
#' 
#' @return List with the portfolio backtest results, see \code{\link{portfolioBacktest}}.
#' 
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' uniform_portfolio <- function(dataset) {
#'   N <- ncol(dataset$adjusted)
#'   return(rep(1/N, N))
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), dataset10)
#' 
#' # add a new performance measure
#' bt <- add_performance(bt, name = "SR arithmetic", 
#'                       fun = function(return, ...) 
#'                                PerformanceAnalytics::SharpeRatio.annualized(return, 
#'                                                                             geometric = FALSE))
#'                                
#' bt <- add_performance(bt, name = "avg leverage", desired_direction = -1,
#'                       fun = function(w_bop, ...)
#'                                if(anyNA(w_bop)) NA else mean(rowSums(abs(w_bop))))
#' }
#' 
#' @export
add_performance <- function(bt, name, fun, desired_direction = 1) {
  
  each_dataset <- function(bt_single) {
    judge_tmp <- attr(bt_single$performance, "judge")
    names_tmp <- names(bt_single$performance)
    bt_single$performance <- c(bt_single$performance,
                               do.call(fun, bt_single))
    names(bt_single$performance) <- c(names_tmp, name)
    attr(bt_single$performance, "judge") <- c(judge_tmp, desired_direction)
    return(bt_single)
  }
  
  each_portfolio <- function(bt_portfolio)
    lapply(bt_portfolio, each_dataset)
  
  bt_attributes <- attributes(bt)
  bt <- lapply(bt, each_portfolio)
  attributes(bt) <- bt_attributes
  return(bt)
}
