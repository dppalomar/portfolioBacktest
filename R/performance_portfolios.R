
#' @import stats
portfolioPerformance <- function(rets, bars_per_year, 
                                 rebalances_per_period = NULL,
                                 turnover_per_period = NULL, 
                                 ROT_bps = NULL) {
  # create empty return object
  performance <- c("Sharpe ratio" = NA, "max drawdown" = NA, "annual return" = NA, "annual volatility" = NA,
                   "Sortino ratio" = NA, "downside deviation" = NA,
                   "Sterling ratio" = NA, "Omega ratio" = NA, "VaR (0.95)" = NA, "CVaR (0.95)" = NA,
                   "rebalancing period" = NA, "turnover" = NA, "ROT (bps)" = NA)
  attr(performance, "desired_direction") <- c(+1, -1, +1, -1, 
                                              +1, -1,
                                              +1, +1, -1, -1, 
                                              -1, -1, +1)
  
  if (!anyNA(rets)) {
    fraction_in <- sum(abs(rets) > 1e-8)/nrow(rets)
    rets <- rets[abs(rets) > 1e-8]  # remove data where return is zero
    if (nrow(rets) == 0)
      performance[c("max drawdown", "annual return", "annual volatility", "VaR (0.95)", "CVaR (0.95)")] <- 0
    else {
      performance["Sharpe ratio"]       <- PerformanceAnalytics::SharpeRatio.annualized(rets, scale = bars_per_year, geometric = FALSE) * sqrt(fraction_in)
      #performance["Sharpe ratio"]       <- sqrt(bars_per_year) * PerformanceAnalytics::SharpeRatio(rets, FUN = "StdDev") * sqrt(fraction_in)
      #performance["Prob. Sharpe ratio"] <- sqrt(bars_per_year * PerformanceAnalytics::ProbSharpeRatio(rets, refSR = 0)$sr_prob
      performance["max drawdown"]       <- PerformanceAnalytics::maxDrawdown(rets)
      #performance["annual return"]      <- PerformanceAnalytics::Return.annualized(rets, scale = bars_per_year, geometric = FALSE) * fraction_in
      performance["annual return"]      <- bars_per_year * fraction_in * mean(rets, na.rm = TRUE)        # prod(1 + rets)^(252/nrow(rets)) - 1 or mean(rets) * 252
      #performance["annual volatility"]  <- PerformanceAnalytics::StdDev.annualized(rets, scale = bars_per_year) * sqrt(fraction_in)  # sqrt(252) * sd(rets, na.rm = TRUE)
      performance["annual volatility"]  <- sqrt(bars_per_year * fraction_in) * sd(rets, na.rm = TRUE)
      performance["Sortino ratio"]      <- sqrt(bars_per_year) * PerformanceAnalytics::SortinoRatio(rets) #, MAR = mean(rets))
      performance["downside deviation"] <- sqrt(bars_per_year) * PerformanceAnalytics::DownsideDeviation(rets) #, MAR = mean(rets))  # same as SemiDeviation()
      performance["Sterling ratio"]     <- performance["annual return"] / performance["max drawdown"]
      performance["Omega ratio"]        <- PerformanceAnalytics::Omega(rets)
      performance["VaR (0.95)"]         <- PerformanceAnalytics::VaR(rets, 0.95, method = "historical", invert = FALSE)
      performance["CVaR (0.95)"]        <- PerformanceAnalytics::CVaR(rets, 0.95, method = "historical", invert = FALSE)
      if (!is.null(rebalances_per_period))
        performance["rebalancing period"] <- 1/rebalances_per_period
      if (!is.null(turnover_per_period))
        performance["turnover"]         <- turnover_per_period
      if (!is.null(ROT_bps))
        performance["ROT (bps)"]        <- ROT_bps
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
#' @author Daniel P. Palomar
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' EWP_portfolio <- function(dataset, ...) {
#'   N <- ncol(dataset$adjusted)
#'   return(rep(1/N, N))
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("EWP" = EWP_portfolio), dataset10)
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
  
  fun_each_dataset <- function(bt_single) {
    judge_tmp <- attr(bt_single$performance, "desired_direction")
    names_tmp <- names(bt_single$performance)
    bt_single$performance <- c(bt_single$performance,
                               do.call(fun, bt_single))
    names(bt_single$performance) <- c(names_tmp, name)
    attr(bt_single$performance, "desired_direction") <- c(judge_tmp, desired_direction)
    return(bt_single)
  }
  
  fun_each_portfolio <- function(bt_portfolio)
    lapply(bt_portfolio, fun_each_dataset)
  
  bt_attributes <- attributes(bt)
  bt <- lapply(bt, fun_each_portfolio)
  attributes(bt) <- bt_attributes
  return(bt)
}




#' @title List portfolios with failures
#'
#' @param bt Backtest results as produced by the function \code{\link{portfolioBacktest}}.
#' 
#' @author Daniel P. Palomar
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' portfolio_with_errors <- function(dataset, ...) {
#'   return(NA)
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("Portfolio with errors" = portfolio_with_errors), dataset10)
#' 
#' listPortfoliosWithFailures(bt)
#' }
#' 
#' @export
listPortfoliosWithFailures <- function(bt) {
  summary_bt <- backtestSummary(bt)
  summary_bt_failures <- summaryTable(summary_bt, measures = "failure rate")
  
  if (any(summary_bt_failures > 0)) {
    cat("ATTENTION: The following portfolios have failures: \n")
    cat(dQuote(rownames(summary_bt_failures)[summary_bt_failures > 0]), sep = ", ")
    cat("\n\nIn particular, in the following backtests: \n")
    for (i in which(summary_bt_failures > 0)) {
      cat(paste0(dQuote(names(bt)[i]), ": "))
      cat(which(sapply(bt[[i]], function(x) x$error)), sep = ", ")
      cat("\n")
    }
  }
}


