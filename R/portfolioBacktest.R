# Backtesting on a single xts
#
#' @import xts
#'         PerformanceAnalytics
singlePortfolioBacktest <- function(portfolio_fun, prices,
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
  
  start_time <- Sys.time() # time the following procedure
  # compute w
  error <- FALSE
  error_message <- NULL
  w <- xts(matrix(NA, length(rebalancing_indices), N), order.by = index(prices)[rebalancing_indices])
  colnames(w) <- colnames(prices)
  for (i in 1:length(rebalancing_indices)) {
    idx_prices <- rebalancing_indices[i]
    prices_window <- prices[(idx_prices-T_sliding_window+1):idx_prices, ]
    tryCatch({w[i, ] <- do.call(portfolio_fun, list(prices_window))},
             warning = function(w) { error <<- TRUE; error_message <<- w$message},
             error = function(e) { error <<- TRUE; error_message <<- e$message})
    # exit in case of error
    if (!error) {
      if (!shortselling && any(w[i, ] + 1e-8 < 0)) {
        error <- TRUE
        error_message <- "no-shortselling constraint not satisfied"
      } else if (sum(abs(w[i, ])) > leverage + 1e-8) {
        error <- TRUE
        error_message <- "budget/leverage constraint not satisfied"
      }
    }
    if (error) return(list("returns" = NA,
                           "cumPnL" = NA,
                           "performance" = rep(NA, 4),
                           "time" = NA,
                           "error" = error,
                           "error_message" = error_message))
  }
  
  time <- as.numeric(Sys.time() - start_time)
  
  # compute returns of portfolio
  R_lin <- PerformanceAnalytics::CalculateReturns(prices[-c(1:(T_sliding_window-1)), ])
  rets <- returnPortfolio(R = R_lin, weights = w)
  
  # compute cumulative wealth (initial budget of 1$)
  #wealth_arith_BnH_trn <- 1 + cumsum(rets)
  wealth_geom_BnH_trn <- cumprod(1 + rets)
  
  # compute various performance measures (in the future, add turnover and ROI)
  performance <- c(SharpeRatio.annualized(rets), maxDrawdown(rets), Return.annualized(rets), StdDev.annualized(rets))
  names(performance) <- c("sharpe ratio", "max drawdown", "expected return", "volatility")
  
  return(list("returns" = rets,
              "cumPnL" = wealth_geom_BnH_trn,
              "performance" = performance,
              "time" = time,
              "error" = error,
              "error_message" = error_message))
}


#' @title Backtesting Portfolio Design on a Rolling-Window Basis of Set of Prices
#'
#' @description Backtest a portfolio design contained in a function on a rolling-window basis of a set of prices.
#'
#' @param portfolio_fun function that takes as input an \code{xts} containing the stock prices and returns the portfolio weights.
#' @param prices an xts object (or a list of \code{xts}) containing the stock prices for the backtesting.
#' @param shortselling whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage).
#' @param T_sliding_window length of the sliding window.
#' @param freq_optim how often the portfolio is to be reoptimized.
#' @param freq_rebalance how often the portfolio is to be rebalanced.
#' @return A list containing the performance in the following elements:
#' \item{\code{returns}  }{xts object (or a list of xts when \code{prices} is a list), the daily return of given portfolio function}
#' \item{\code{cumPnL}  }{xts object (or a list of xts when \code{prices} is a list), the cummulative daily return of given portfolio function}
#' \item{\code{performance}}{matrix, each row is responding to one data example in order}
#' \item{\code{performance_summary}}{vector, summarizing the performance by its median value (only returned when argument \code{prices} is a list of xts)}
#' \item{\code{time}}{vector, recording time for execute the portfolio for one data examples}
#' \item{\code{time_average}}{number, the average of \code{time} but ignoring the NAs (only returned when argument \code{prices} is a list of xts)}
#' \item{\code{failure_ratio}}{number, the failure ratio of applying given portfolio function to different data examples (only returned when argument \code{prices} is a list of xts)}
#' \item{\code{error}}{logical vector, indicating whether an error happens in data examples}
#' \item{\code{error_message}}{string list, recording the error message when error happens}
#' @author Daniel P. Palomar and Rui Zhou
#' @examples
#' library(xts)
#' library(backtestPortfolio)
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
#'   w <- w/sum(abs(w))  # normalized to have ||w||_1=1
#'   return(w)
#' }
#' 
#' # perform backtesting on one xts
#' res <- portfolioBacktest(portfolio_fun, prices[[1]], shortselling = TRUE)
#' names(res)
#' plot(res$cumPnL)
#' res$performance
#'
#' # perform backtesting on a list of xts
#' mul_res <- portfolioBacktest(portfolio_fun, prices, shortselling = TRUE)
#' mul_res$performance
#' mul_res$performance_summary
#' 
#' @export
portfolioBacktest <- function(portfolio_fun, prices, ...) {
  
  # when price is an xts object
  if (!is.list(prices)) {
    return(singlePortfolioBacktest(portfolio_fun = portfolio_fun, prices = prices, ...))
  }
  
  rets <- cumPnL <- performance <- error_message <- list()
  time <- error <- c()
  
  # when price is a list of xts object
  for (i in 1:length(prices)) {
    result <- singlePortfolioBacktest(portfolio_fun = portfolio_fun, prices = prices[[i]], ...)
    rets[[i]] <- result$return
    cumPnL[[i]] <- result$cumPnL
    performance[[i]] <- result$performance
    time[i] <- result$time
    error[i] <- result$error
    error_message[[i]] <- result$error_message
  }
  
  # prepare results to be returned
  performance <- sapply(performance, cbind)
  rownames(performance) <- names(result$performance)
  # summarize performance 
  failure_ratio <- sum(error) / length(prices)
  if (failure_ratio < 1) {
    performance_summary <- apply(performance[, !error], 1, median)
    names(performance_summary) <- paste(names(performance_summary), "(median)")
    time_average <- mean(time[!error])
  } else {
    performance_summary <- time_average <- NA
  }

  return(list("returns" = rets,
              "cumPnL" = cumPnL,
              "performance" = performance,
              "performance_summary" = performance_summary,
              "time" = time,
              "time_average" = time_average,
              "failure_ratio" = failure_ratio,
              "error" = error,
              "error_message" = error_message))
}


#
# Computes the returns of a portfolio of several assets (ignoring transaction costs):
#   R: is an xts with the individual asset linear returns (not log returns)
#   weights: is an xts with the normalized dollar allocation (wrt NAV, typically with sum=1) where
#            - each row represents a rebalancing date (with portfolio computed with info up to and including that day)
#            - dates with no rows means no rebalancing (note that the portfolio may then violate some margin constraints...)
#
#' @import xts
returnPortfolio <- function(R, weights, execution = c("same day", "next day"), name = "portfolio.returns") {
  ######## error control  #########
  if (!is.xts(R) || !is.xts(weights)) stop("This function only accepts xts")
  if (attr(index(R), "class") != "Date") stop("This function only accepts daily data")
  if (!all(index(weights) %in% index(R))) stop("Weight dates do not appear in the returns")
  if (ncol(R) != ncol(weights)) stop("Number of weights does not match the number of assets in the returns")
  if (anyNA(R[-1])) stop("Returns contain NAs")
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

