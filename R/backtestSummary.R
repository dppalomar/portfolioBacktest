#' @title Portfolio Backtest Results Summary
#' 
#' @description Summarize the results from portfolio backtest
#' 
#' @param res the results from function `portfolioBacktest()`
#' @param portfolio_names the names of a portfolio
#' @param portfolio_indexs the indexs of a portfolio
#' @param summary_funs a list of summary function
#' @param show_benchmark logical value indicating whether to show benchmark in the portfolio
#' 
#' @return a list of desired results
#' 
#' @author Daniel P. Palomar and Rui Zhou
#'
#' @export  


backtestSummary <- function(res, portfolio_names = NA, portfolio_indexs = NA,
                            summary_funs = list(median = median), show_benchmark = TRUE) {
  if (anyNA(portfolio_names) && anyNA(portfolio_indexs)) portfolio_indexs <- attr(res, 'portfolio_index')
  if (!anyNA(portfolio_indexs)) portfolio_names <- names(res)[portfolio_indexs]
  if (show_benchmark) portfolio_names <- c(portfolio_names, names(res)[attr(res, 'benchmark_index')])
  
  n_portfolio <- length(portfolio_names)
  result <- list()
  summary_container <- matrix(NA, n_portfolio, length(portfolioPerformance()))
  for (i in 1:length(summary_funs)) {
    summary_name <- paste0('performance_summary_', names(summary_funs)[i])
    tmp <- backtestSummarySingleFun(res, portfolio_names, summary_funs[[i]])
    result[[summary_name]] <- tmp$performance
  }
  
  # add cpu time information
  result$cpu_time_average <- tmp$cpu_time_average
  names(result$cpu_time_average) <- portfolio_names
  
  # add error information
  result$failure_rate <- tmp$failure_rate
  names(result$failure_rate) <- portfolio_names
  result$error_message <- tmp$error_message
  names(result$error_message) <- portfolio_names
  
  return(result)
}


backtestSummarySingleFun <- function(res, portfolio_names, summary_fun) {
  n_portfolio <- length(portfolio_names)
  result <- error_message <- list()
  failure_rate <- cpu_time_average <- c()
  summary_container <- matrix(NA, n_portfolio, length(portfolioPerformance()))
  colnames(summary_container) <- names(portfolioPerformance())
  rownames(summary_container) <- portfolio_names
  for (i in 1:n_portfolio) {
    tmp <- backtestSelector(res, portfolio_names[i])
    if (!is.null(tmp$source_error_message)) {
      failure_rate <- c(failure_rate, 1)
      error_message[[i]] <- tmp$source_error_message
      cpu_time_average <- c(cpu_time_average, NA)
      next
    }
    mask_fail <- tmp$error
    summary_container[i, ] <- apply(tmp$performance[!mask_fail, ], 2, summary_fun)
    failure_rate <- c(failure_rate, mean(mask_fail))
    cpu_time_average <- c(cpu_time_average, mean(tmp$cpu_time[!mask_fail]))
    err_mess <- unique(unlist(tmp$error_message))
    error_message[[i]] <- err_mess[!is.na(err_mess)]
  }
  cpu_time_average[is.nan(cpu_time_average)] <- NA
  return(list(performance_summary = summary_container, 
              cpu_time_average = cpu_time_average,
              failure_rate = failure_rate,
              error_message = error_message))
}




#' @title Portfolio Backtest Results Selector
#' 
#' @description Select the results from portfolio backtest and return as a matrix
#' 
#' @param res the results from function `portfolioBacktest()`
#' @param portfolio_name the name of a portfolio
#' @param portfolio_index the index of a portfolio
#' @param selector a vector of required performance
#' 
#' @return a list of desired results
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @export

backtestSelector <- function(res, portfolio_name = names(res)[1], portfolio_index = NA, selector = NULL) {
  selector_range <- c(names(portfolioPerformance()), 'error', 'error_message', 'cpu_time', 'return', 'portfolio')
  if (is.null(selector)) selector <- selector_range
  if (any(!(selector %in% selector_range))) stop("\"selector\" contains invalid element ")
  if (length(selector) == 0) stop("\"selector\" must have length > 1")
  if (!is.na(portfolio_index)) portfolio_name <- names(res)[portfolio_index]
  if (!is.null(res[[portfolio_name]]$source_error_message)) return(res[[portfolio_name]])
  
  result <- list()
  mask_performance <- setdiff(selector, c('error', 'error_message', 'cpu_time', 'return', 'portfolio'))
  if (length(mask_performance) > 0)
    result$performance <- t(sapply(res[[portfolio_name]], function(x){x$performance[mask_performance]}))
  if ('error' %in% selector) 
    result$error <- sapply(res[[portfolio_name]], function(x){x$error})
  if ('error_message' %in% selector) 
    result$error_message <- lapply(res[[portfolio_name]], function(x){x$error_message})
  if ('cpu_time' %in% selector)
    result$cpu_time <- sapply(res[[portfolio_name]], function(x){x$cpu_time})
  if ('portfolio' %in% selector)
    result$portfolio <- lapply(res[[portfolio_name]], function(x){x$portfolio})
  if ('return' %in% selector) {
    result$return <- lapply(res[[portfolio_name]], function(x){x$return})
    result$cumPnL <- lapply(res[[portfolio_name]], function(x){x$cumPnL})
  }
  
  return(result)
}
