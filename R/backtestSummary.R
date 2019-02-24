#' @title Portfolio Backtest Results Summary
#' 
#' @description Summarize the results from portfolio backtest
#' 
#' @param res the results from function `portfolioBacktest()`
#' @param portfolio_names the names of a portfolio
#' @param portfolio_indexs the indexs of a portfolio
#' @param summary_funs summary function
#' @param show_benchmark logical value indicating whether to show benchmark in the portfolio
#' 
#' @return a list of desired results
#' 
#' @author Daniel P. Palomar and Rui Zhou
#'
#' @export  


backtestSummary <- function(res, portfolio_names = NA, portfolio_indexs = NA,
                            summary_fun = median, show_benchmark = TRUE) {
  if (anyNA(portfolio_names) && anyNA(portfolio_indexs)) portfolio_indexs <- attr(res, 'portfolio_index')
  if (!anyNA(portfolio_indexs)) portfolio_names <- names(res)[portfolio_indexs]
  if (show_benchmark) portfolio_names <- c(portfolio_names, names(res)[attr(res, 'benchmark_index')])
  
  n_portfolio <- length(portfolio_names)
  result <- list()
  summary_container <- matrix(NA, n_portfolio, length(portfolioPerformance()))
  
  performance <- failure_rate <- cpu_time_summary <- list()
  res_table <- backtestTable(res)
  for (portfolio_name in portfolio_names) {
    tmp <- backtestSummarySinglePortfolio(res_table, portfolio_name, summary_fun)
    performance[[portfolio_name]] <- tmp$performance
    failure_rate[[portfolio_name]] <- tmp$failure_rate
    cpu_time_summary[[portfolio_name]] <- tmp$cpu_time_summary
  }
  
  rt <- list()
  rt$performance_summary <- cbind(Reduce(cbind, performance))
  rt$failure_rate <- unlist(failure_rate)
  rt$cpu_time_summary <- unlist(cpu_time_summary)
  rt$error_message <- res_table$error_message
  
  colnames(rt$performance_summary) <- names(rt$failure_rate) <- names(rt$cpu_time_summary) <- portfolio_names
  rownames(rt$performance_summary) <- names(portfolioPerformance())
  return(rt)
}


backtestSummarySinglePortfolio <- function(res_table, portfolio_name, summary_fun) {
  # assume the res_table contains all performance metric
  performance <- portfolioPerformance()
  mask_performance <- names(performance)
  fail_mask <- res_table$error[, portfolio_name]
  failure_rate <- mean(fail_mask)
  cpu_time_summary <- NA
  if (failure_rate < 1) {
    for (metric in mask_performance)
      performance[metric] <- summary_fun(res_table[[metric]][!fail_mask, portfolio_name])
    cpu_time_summary <- summary_fun(res_table$cpu_time[!fail_mask, portfolio_name])
  }
  return(list(performance = performance, 
              failure_rate = failure_rate,
              cpu_time_summary = cpu_time_summary))
}

#' @title Portfolio Backtest Results in Table form
#' 
#' @description Show the results from portfolio backtest in tables
#' 
#' @param res_table the results from function `portfolioBacktest()`
#' @param portfolio_names the names of a portfolio
#' @param portfolio_indexs the indexs of a portfolio
#' @param show_benchmark logical value indicating whether to show benchmark in the portfolio
#' @param selector a vector of required performance
#' @return a list of desired results
#' 
#' @author Daniel P. Palomar and Rui Zhou
#'
#' @export  
#' 
backtestTable <- function(res, portfolio_names = NA, portfolio_indexs = NA, 
                          show_benchmark = TRUE, selector = NULL) {
  # check portfolio index and names
  if (anyNA(portfolio_names) && anyNA(portfolio_indexs)) portfolio_indexs <- attr(res, 'portfolio_index')
  if (!anyNA(portfolio_indexs)) portfolio_names <- names(res)[portfolio_indexs]
  if (show_benchmark) portfolio_names <- c(portfolio_names, names(res)[attr(res, 'benchmark_index')])
  
  # check selector
  selector_range <- c(names(portfolioPerformance()), 'error', 'error_message', 'cpu_time')
  if (is.null(selector)) selector <- selector_range
  if (any(!(selector %in% selector_range))) stop("\"selector\" contains invalid element")
  
  # check if source_error happen
  valid_mask <- sapply(res[portfolio_names], function(x){is.null(x$source_error_message)})
  if (!any(valid_mask)) stop("all files fail to be sourced")
  
  # extract results and combine into matrix
  N_dataset <- length(res[[portfolio_names[valid_mask][1]]])
  N_portfolio <- length(portfolio_names)
  mask_performance <- setdiff(selector, c('error', 'error_message', 'cpu_time'))
  
  
  container <- matrix(NA, N_dataset, N_portfolio)
  colnames(container) <- portfolio_names
  rownames(container) <- names(res[[1]])
  cpu_time <- error <- container
  performance <- error_message <- list()
  
  # fill in all results
  for (i in 1:N_portfolio) {
    
    tmp <- backtestSelector(res = res, portfolio_name = portfolio_names[i], selector = selector)
    
    for (metric in mask_performance) {
      # creat space in first visit
      if (i == 1) performance[[metric]] <- container
      # check source error
      if (!valid_mask[i]) next
      # fill in certain metric
      performance[[metric]][, i] <- tmp$performance[, metric]
    }
    
    if ('error' %in% selector)
      if (valid_mask[i]) 
        error[, i] <- tmp$error
      else
        error[, i] <- TRUE
      
    if ('cpu_time' %in% selector)
      if (valid_mask[i])
        cpu_time[, i] <- tmp$cpu_time
    
    if ('error_message' %in% selector)
      if (valid_mask[i])
        error_message[[portfolio_names[i]]] <- tmp$error_message
      else
        error_message[[portfolio_names[i]]] <- res[[portfolio_names[i]]]$source_error_message
  }
  
  rt <- list()
  if (length(mask_performance) >= 1) rt <- performance
  if ('error' %in% selector)         rt$error <- error
  if ('cpu_time' %in% selector)      rt$cpu_time <- cpu_time
  if ('error_message' %in% selector) rt$error_message <- error_message
  
  return(rt)
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
backtestSelector <- function(res, portfolio_name = NA, portfolio_index = NA, selector = NULL) {
  selector_range <- c(names(portfolioPerformance()), 'error', 'error_message', 'cpu_time', 'return', 'portfolio')
  if (is.null(selector)) selector <- selector_range
  if (any(!(selector %in% selector_range))) stop("\"selector\" contains invalid element")
  if (length(selector) == 0) stop("\"selector\" must have length > 1")
  if (is.na(portfolio_name) && is.na(portfolio_index)) stop("must select a portfolio") 
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
