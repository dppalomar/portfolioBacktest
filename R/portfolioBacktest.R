#' @title Backtest multiple portfolios over multiple datasets of stock prices in a rolling-window basis.
#'
#' @description Automated backtesting of multiple portfolios over multiple 
#' datasets of stock prices in a rolling-window fashion. 
#' Each portfolio design is easily defined as a
#' function that takes as input a window of the stock prices and outputs the 
#' portfolio weights. Multiple portfolios can be easily specified as a list 
#' of functions or as files in a folder. Multiple datasets can be conveniently 
#' obtained with the function \code{\link{stockDataResample}} that resamples
#' the data downloaded with the function \code{\link{stockDataDownload}}.
#' The results can be later assessed and arranged with tables and plots.
#' The backtesting can be highly time-consuming depending on the number of 
#' portfolios and datasets can be performed with parallel computation over
#' multiple cores. Errors in functions are properly catched and handled so
#' that the execution of the overal backtesting is not stopped (error messages
#' are stored for debugging purposes). See 
#' \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html}{vignette}
#' for a detailed explanation.
#'
#' @param portfolio_funs List of functions (can also be a single function), each of them taking as input 
#'                       a dataset containing a list of \code{xts} objects (following the format of each 
#'                       element of the argument \code{dataset_list}) properly windowed (following the
#'                       rolling-window approach) and returning the portfolio as a vector of normalized
#'                       weights. See 
#'                       \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html}{vignette}
#'                       for details.
#' @param dataset_list List of datasets, each containing a list of \code{xts} objects, as generated
#'                     by the function \code{\link{stockDataResample}}.
#' @param folder_path If \code{portfolio_funs} is not defined, this should contain the path to a folder 
#'                    containing the portfolio functions saved in files. See 
#'                    \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html}{vignette}
#'                    for details.
#' @param price_name Name of the \code{xts} column in each dataset that contains the prices to be used in the portfolio return 
#'                   computation (default is \code{"adjusted"}).
#' @param paral_portfolios Interger indicating number of portfolios to be evaluated in parallel (default is \code{1}).
#' @param paral_datasets Interger indicating number of datasets to be evaluated in parallel (default is \code{1}).
#' @param show_progress_bar Logical value indicating whether to show progress bar (default is \code{FALSE}). 
#' @param benchmark String vector indicating the benchmark portfolios to be incorporated, currently supports:
#' \itemize{\item{\code{uniform} - the uniform portfolio, \eqn{w = [1/N, ..., 1/N]} with \eqn{N} be number of stocks}
#'          \item{\code{index} - the market index, requires an \code{xts} named `index` in the datasets.}}
#' @param shortselling Logical value indicating whether shortselling is allowed or not 
#'                     (default is \code{TRUE}, so no control for shorselling in the backtesting).
#' @param leverage Amount of leverage as in \eqn{||w||_1 <= leverage} 
#'                 (default is \code{Inf}, so no control for leverage in the backtesting).
#' @param T_rolling_window Length of the lookback rolling window (default is \code{252}).
#' @param optimize_every How often the portfolio is to be optimized (default is \code{20}).
#' @param rebalance_every How often the portfolio is to be rebalanced (default is \code{1}).
#' @param cpu_time_limit Time limit for executing each portfolio function over a single data set 
#'                       (default is \code{Inf}, so no time limit).
#' @param return_portfolio Logical value indicating whether to return portfolios (default is \code{TRUE}).
#' @param return_returns Logical value indicating whether to return the portfolio returns (default is \code{TRUE}).
#' 
#' @return List with the portfolio backtest results, see 
#'         \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html#result-format}{vignette-result-format}
#'         for details. It can be accessed directly, but we highly recommend the package specific functions to extract any required
#'         information, namely, \code{\link{backtestSelector}}, \code{\link{backtestTable}}, 
#'         \code{\link{backtestBoxPlot}}, \code{\link{backtestLeaderboard}},
#'         \code{\link{backtestSummary}}, \code{\link{summaryTable}}, \code{\link{summaryBarPlot}}.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{stockDataDownload}}, \code{\link{stockDataResample}},
#'          \code{\link{backtestSelector}}, \code{\link{backtestTable}}, 
#'          \code{\link{backtestBoxPlot}}, \code{\link{backtestLeaderboard}},
#'          \code{\link{backtestSummary}}, \code{\link{summaryTable}}, \code{\link{summaryBarPlot}}.
#' 
#' @examples
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
#' # check your result
#' names(bt)
#' backtestSelector(bt, portfolio_name = "Uniform", measures = c("Sharpe ratio", "max drawdown"))
#' backtestTable(bt, measures = c("Sharpe ratio", "max drawdown"))
#' bt_summary <- backtestSummary(bt)
#' summaryTable(bt_summary)
#' 
#' @import xts
#' @importFrom zoo index
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom snow makeCluster stopCluster
#' @export
portfolioBacktest <- function(portfolio_funs = NULL, dataset_list, folder_path = NULL, price_name = "adjusted",
                              paral_portfolios = 1, paral_datasets = 1,
                              show_progress_bar = FALSE, benchmark = NULL, 
                              shortselling = TRUE, leverage = Inf,
                              T_rolling_window = 252, optimize_every = 20, rebalance_every = 1,
                              cpu_time_limit = Inf,
                              return_portfolio = TRUE, return_returns = TRUE) {
  ####### error control ########
  paral_portfolios <- round(paral_portfolios)
  paral_datasets <- round(paral_datasets)
  if (paral_portfolios < 1 || paral_datasets < 1) stop("Parallel number must be a positive interger.")
  if (is.null(folder_path) && is.null(portfolio_funs)) stop("The \"folder_path\" and \"portfolio_fun_list\" cannot be both NULL.")
  if (!is.null(portfolio_funs) && !is.list(portfolio_funs)) portfolio_funs <- list(portfolio_funs)
  ##############################
  
  # when portfolio_funs is passed
  if (!is.null(portfolio_funs)) {
    if (is.null(names(portfolio_funs))) portfolio_names <- paste0("fun", 1:length(portfolio_funs))
    else portfolio_names <- names(portfolio_funs)
    
    if ("" %in% portfolio_names) stop("Each element of \"portfolio_funs\" must has a unique name.")
    if (length(portfolio_names) != length(unique(portfolio_names))) stop("\"portfolio_funs\" contains repeated names.")
    
    # in case the extra packages are loaded
    safeEvalPortf <- function(portfolio_fun, dataset_list, price_name,
                              paral_datasets, show_progress_bar,
                              shortselling, leverage,
                              T_rolling_window, optimize_every, rebalance_every,
                              cpu_time_limit,
                              return_portfolio, return_returns) {
      packages_default <- search()  # snap the default packages
      res <- singlePortfolioBacktest(portfolio_fun, dataset_list, price_name, market = FALSE,
                                     paral_datasets, show_progress_bar,
                                     shortselling, leverage,
                                     T_rolling_window, optimize_every, rebalance_every,
                                     cpu_time_limit,
                                     return_portfolio, return_returns)
      packages_now <- search()  # detach the newly loaded packages
      packages_det <- packages_now[!(packages_now %in% packages_default)]
      detachPackages(packages_det)
      return(res)
    }
    
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(portfolio_funs)) {
        if (show_progress_bar) 
          message("\n Backtesting function ", format(portfolio_names[i], width = 15), " (", i, "/", length(portfolio_names), ")")
        result[[i]] <- safeEvalPortf(portfolio_funs[[i]], dataset_list, price_name,
                                     paral_datasets, show_progress_bar,
                                     shortselling, leverage,
                                     T_rolling_window, optimize_every, rebalance_every,
                                     cpu_time_limit,
                                     return_portfolio, return_returns)
      }
    } else {
      # create the progress bar based on function number
      if (show_progress_bar) {
        sink(file = tempfile())
        pb <- utils::txtProgressBar(max = length(portfolio_funs), style = 3)
        sink()
        opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
        message("Evaluating overall ", length(portfolio_funs), " portfolio functions in parallel")
        opts$progress(0)
      } else opts <- list()
      
      cl <- makeCluster(paral_portfolios)
      registerDoSNOW(cl)
      exports <- ls(envir = .GlobalEnv)
      exports <- exports[! exports %in% c("portfolio_fun", "dataset_list", "show_progress_bar")]
      portfolio_fun <- NULL  # ugly hack to deal with CRAN note
      result <- foreach(portfolio_fun = portfolio_funs, .combine = c, .export = exports, 
                        .packages = .packages(), .options.snow = opts) %dopar% {
        return(list(safeEvalPortf(portfolio_fun, dataset_list, price_name,
                                  paral_datasets, show_progress_bar,
                                  shortselling, leverage,
                                  T_rolling_window, optimize_every, rebalance_every,
                                  cpu_time_limit,
                                  return_portfolio, return_returns)))
      }
      if (show_progress_bar) close(pb)
      stopCluster(cl) 
    }
    
  } else { # when folder_path is passed
    files <- list.files(folder_path)
    portfolio_names <- gsub(".R", "", files)
    if (length(portfolio_names) == 0) stop(sprintf("Could not find any .R files in folder: %s", folder_path))

    # define a safe enviroment to source .R files
    dataset_list__ <- dataset_list  # backup dataset_list in case of being covered by sourced files
    safeEvalFolder <- function(folder_path, file, dataset_list__, price_name,
                               paral_datasets, show_progress_bar,
                               shortselling, leverage,
                               T_rolling_window, optimize_every, rebalance_every,
                               cpu_time_limit,
                               return_portfolio, return_returns) {
      packages_default <- search()  # snap the default packages
      
      error_message <- tryCatch({
        suppressMessages(source(file.path(folder_path, file), local = TRUE))
        res <- singlePortfolioBacktest(portfolio_fun, dataset_list__, price_name, market = FALSE,
                                       paral_datasets, show_progress_bar,
                                       shortselling, leverage,
                                       T_rolling_window, optimize_every, rebalance_every,
                                       cpu_time_limit,
                                       return_portfolio, return_returns)
        NULL
      }, warning = function(w) return(ifelse(!is.null(w$message), w$message, ""))
       , error   = function(e) return(ifelse(!is.null(e$message), e$message, ""))
      )     
      packages_now <- search()  # detach the newly loaded packages
      packages_det <- packages_now[!(packages_now %in% packages_default)]
      detachPackages(packages_det)
      if (!is.null(error_message)) return(list(source_error_message = error_message))
      return(res)
    }
    
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(files)) {
        if (show_progress_bar)
          message("\n Backtesting file ", format(portfolio_names[i], width = 15), " (", i, "/", length(portfolio_names), ")")
        result[[i]] <- safeEvalFolder(folder_path, files[i], dataset_list__, price_name,
                                      paral_datasets, show_progress_bar,
                                      shortselling, leverage,
                                      T_rolling_window, optimize_every, rebalance_every,
                                      cpu_time_limit,
                                      return_portfolio, return_returns)
      }
    } else {
      # creat the progress bar based on files number
      if (show_progress_bar) {
        sink(file = tempfile())
        pb <- utils::txtProgressBar(max = length(files), style = 3)
        sink()
        opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
        message("Evaluating overall ", length(files), " portfolio functions (from files) in parallel")
        opts$progress(0)
      } else opts <- list()
      
      cl <- makeCluster(paral_portfolios)
      registerDoSNOW(cl)
      file <- NULL  # ugly hack to deal with CRAN note
      result <- foreach(file = files, .combine = c, .options.snow = opts) %dopar% {
        return(list(safeEvalFolder(folder_path, file, dataset_list__, price_name,
                                   paral_datasets, show_progress_bar,
                                   shortselling, leverage,
                                   T_rolling_window, optimize_every, rebalance_every,
                                   cpu_time_limit,
                                   return_portfolio, return_returns)))
      }
      if (show_progress_bar) close(pb)
      stopCluster(cl) 
    }
  }
  
  names(result) <- portfolio_names
  
  # add benchmark and return result
  res_benchmark <- benchmarkBacktest(dataset_list, benchmark, price_name,
                                     paral_datasets, show_progress_bar,
                                     shortselling, leverage,
                                     T_rolling_window, optimize_every, rebalance_every,
                                     cpu_time_limit,
                                     return_portfolio, return_returns)
  result <- c(result, res_benchmark)
  attr(result, 'portfolio_index') <- 1:length(portfolio_names)
  attr(result, 'contain_benchmark') <- length(res_benchmark) > 0
  attr(result, 'benchmark_index') <- (1:length(result))[-c(1:length(portfolio_names))]
  return(result)
}



# benchmark portfolio backtest
benchmarkBacktest <- function(dataset_list, benchmark, price_name,
                              paral_datasets, show_progress_bar,
                              shortselling, leverage,
                              T_rolling_window, optimize_every, rebalance_every,
                              cpu_time_limit,
                              return_portfolio, return_returns) {
  res <- list()
  if ("uniform" %in% benchmark) {
    if (show_progress_bar) message("\n Evaluating benchmark uniform")
    res$uniform <- singlePortfolioBacktest(uniform_portfolio_fun, dataset_list, price_name, market = FALSE,
                                           paral_datasets, show_progress_bar,
                                           shortselling, leverage,
                                           T_rolling_window, optimize_every, rebalance_every,
                                           cpu_time_limit,
                                           return_portfolio, return_returns)
  }
  if ("index" %in% benchmark) {
    if (show_progress_bar) message("\n Evaluating benchmark index")
    res$index <- singlePortfolioBacktest(portfolio_fun = NULL, dataset_list, price_name, market = TRUE,
                                         paral_datasets, show_progress_bar,
                                         shortselling, leverage,
                                         T_rolling_window, optimize_every, rebalance_every,
                                         cpu_time_limit,
                                         return_portfolio, return_returns)
  }
  return(res)
}



# Backtesting of one portfolio function
singlePortfolioBacktest <- function(portfolio_fun, dataset_list, price_name, market,
                                    paral_datasets, show_progress_bar,
                                    shortselling, leverage,
                                    T_rolling_window, optimize_every, rebalance_every,
                                    cpu_time_limit,
                                    return_portfolio, return_returns) {
  # create the progress bar
  if (show_progress_bar) {
    sink(file = tempfile())
    pb <- utils::txtProgressBar(max = length(dataset_list), style = 3)
    sink()
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
    opts$progress(0)
  } else opts <- list()
  
  # when price is a list of xts object
  if (paral_datasets == 1) { ########## no-parallel mode
    result <- list()
    for (i in 1:length(dataset_list)) {
      result[[i]] <- singlePortfolioSingleXTSBacktest(portfolio_fun, dataset_list[[i]], price_name, market,
                                                      shortselling, leverage,
                                                      T_rolling_window, optimize_every, rebalance_every,
                                                      cpu_time_limit,
                                                      return_portfolio, return_returns)
      if (show_progress_bar) opts$progress(i) # show progress bar
    }
  } else {               ########### parallel mode
    cl <- makeCluster(paral_datasets)
    registerDoSNOW(cl)
    exports <- ls(envir = .GlobalEnv)
    exports <- exports[! exports %in% c("portfolio_fun", "dat")]
    dat <- NULL  # ugly hack to deal with CRAN note
    result <- foreach(dat = dataset_list, .combine = c, .packages = .packages(), .export = ls(envir = .GlobalEnv), .options.snow = opts) %dopar% {
      return(list(singlePortfolioSingleXTSBacktest(portfolio_fun, dat, price_name, market,
                                                   shortselling, leverage,
                                                   T_rolling_window, optimize_every, rebalance_every,
                                                   cpu_time_limit,
                                                   return_portfolio, return_returns)))
    }
    stopCluster(cl) 
  }
  
  if (is.null(names(dataset_list)))
    names(result) <- paste0("data", 1:length(dataset_list))
  else
    names(result) <- names(dataset_list)
  
  return(result)
}



# Backtesting of one portfolio function on one single xts
#
#' @import xts
singlePortfolioSingleXTSBacktest <- function(portfolio_fun, data, price_name, market,
                                             shortselling, leverage,
                                             T_rolling_window, optimize_every, rebalance_every,
                                             cpu_time_limit,
                                             return_portfolio, return_returns) {
  # create return container
  res <- list(performance = portfolioPerformance(), 
              cpu_time = NA, 
              error = FALSE, 
              error_message = NA,
              portfolio = NA,
              return = NA,
              cumPnL = NA)
  
  # when portfolio_return is given
  if (market) {
    idx_prices_window <- data$index[-(1:(T_rolling_window-1))]
    idx_return <- diff(log(idx_prices_window), na.pad = FALSE)
    res$performance <- portfolioPerformance(rets = idx_return)
    res$performance['ROT bps'] <- Inf
    res$cpu_time <- 0
    if (return_returns) {res$return <- idx_return; res$cumPnL <- idx_prices_window[-1]/as.numeric(idx_prices_window[1])}
    return(res)
  }
  
  if (!price_name %in% names(data)) 
    stop("Fail to find price data with name \"", price_name, "\"" , " in given dataset_list.")
  prices <- data[[price_name]]
  
  ######## error control  #########
  if (is.list(prices)) stop("prices have to be xts, not a list, make sure you index the list with double brackets [[.]].")
  if (!is.xts(prices)) stop("prices have to be xts.")
  N <- ncol(prices)
  T <- nrow(prices)
  if (T_rolling_window >= T) stop("T is not large enough for the given sliding window length.")
  if (optimize_every%%rebalance_every != 0) stop("The reoptimization period has to be a multiple of the rebalancing period.")
  if (anyNA(prices)) stop("prices contain NAs.")
  if (!is.function(portfolio_fun)) stop("portfolio_fun is not a function.")
  #################################
  
  # indices
  #rebalancing_indices <- endpoints(prices, on = "weeks")[which(endpoints(prices, on = "weeks") >= T_rolling_window)]
  optimize_indices <- seq(from = T_rolling_window, to = T, by = optimize_every)
  rebalance_indices <- seq(from = T_rolling_window, to = T, by = rebalance_every)
  if (any(!(optimize_indices %in% rebalance_indices))) stop("The reoptimization indices have to be a subset of the rebalancing indices")
  
  
  # compute w
  error <- flag_timeout <- FALSE; error_message <- NA; error_capture <- NULL; cpu_time <- c(); 
  w <- xts(matrix(NA, length(rebalance_indices), N), order.by = index(prices)[rebalance_indices])
  colnames(w) <- colnames(prices)
  
  for (i in 1:length(rebalance_indices)) {
    
    idx_prices <- rebalance_indices[i]
    
    if (idx_prices %in% optimize_indices) {  # reoptimize
      data_window  <- lapply(data, function(x) {x[(idx_prices-T_rolling_window+1):idx_prices, ]})
      start_time <- proc.time()[3] 
      error_capture <- R.utils::withTimeout(expr = evaluate::try_capture_stack(w[i, ] <- do.call(portfolio_fun, list(data_window)), environment()), 
                                            timeout = cpu_time_limit, onTimeout = "silent")
      cpu_time <- c(cpu_time, as.numeric(proc.time()[3] - start_time))
    } else {# just rebalance without reoptimizing
      w[i, ] <- w[i-1, ]
    }
    
    # check if error happened
    if (is.list(error_capture)) {
      error <- TRUE
      error_message <- error_capture$message
      error_stack <- list("at"    = deparse(error_capture$call), 
                          "stack" = paste(sapply(error_capture$calls[-1], deparse), collapse = "\n"))
    }
    
    # check NA
    if (!error && anyNA(w[i, ])) {
      error = TRUE
      error_message <- c("Returned portfolio contains NA.")
    }
    
    # check constraint
    if (!error) {
      if (!shortselling && any(w[i, ] + 1e-6 < 0)) 
        {error <- TRUE; error_message <- c("No-shortselling constraint not satisfied.")}
      if (sum(abs(w[i, ])) > leverage + 1e-6) 
        {error <- TRUE; error_message <- c(error_message, "Leverage constraint not satisfied.")}
    }
    
    # immediate return when error happens
    if (error) {
      res$error <- error
      res$error_message <- error_message[!is.na(error_message)]
      if (is.list(error_capture)) attr(res$error_message, "error_stack") <- error_stack
      if (return_portfolio) res$portfolio <- w
      return(res)
    }
  }
  
  # compute returns of portfolio
  R_lin <- PerformanceAnalytics::CalculateReturns(prices)
  ret_port <- returnPortfolio(R = R_lin, weights = w)
  rets <- ret_port$rets

  res$performance <- portfolioPerformance(rets = rets, ROT_bips = ret_port$ROT_bips)
  res$cpu_time <- mean(cpu_time)
  res$error <- error
  res$error_message <- error_message
  if (return_portfolio) res$portfolio <- w
  if (return_returns) {res$return <- rets; res$cumPnL <- cumprod(1 + rets)}
  
  return(res)
}



# analyse the performance of portfolio returns
#   rets: is an xts recording portfolio's return
#   ROT_bips
#
portfolioPerformance <- function(rets = NA, ROT_bips = NA) {
  performance <- rep(NA, 7)
  names(performance) <- c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", 
                          "Sterling ratio", "Omega ratio", "ROT bps")
  # "judge" means how to judge the performance, 1: the bigger the better, -1: the smaller the better
  attr(performance, "judge") <- c(1, -1, 1, -1, 1, 1, 1)
  
  # return blank vector if no need computation
  if (anyNA(rets)) return(performance)
  
  # fill the elements one by one
  performance["Sharpe ratio"]      <- PerformanceAnalytics::SharpeRatio.annualized(rets)
  performance["max drawdown"]      <- PerformanceAnalytics::maxDrawdown(rets)
  performance["annual return"]     <- PerformanceAnalytics::Return.annualized(rets)
  performance["annual volatility"] <- PerformanceAnalytics::StdDev.annualized(rets)
  performance["Sterling ratio"]    <- PerformanceAnalytics::Return.annualized(rets) / PerformanceAnalytics::maxDrawdown(rets)
  performance["Omega ratio"]       <- PerformanceAnalytics::Omega(rets)
  performance["ROT bps"]           <- ROT_bips
  
  
  return(performance)
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
  switch(match.arg(execution),  # w[t] is the portfolio held before the realization of price[t] and R[t]
         "same day" = { w <- lag.xts(w) },
         "next day" = { w <- lag.xts(w, 2) },
         stop("Execution method unknown")
  )
  rebalance_indices <- which(!is.na(w[, 1]))
  # loop    (NAV contains the NAV at the beginning of the day, like w,
  #          whereas ret contains the returns at the end of the day, so lag(ret) equals (NAV - lag(NAV))/lag(NAV))
  NAV <- ret <- xts(rep(NA, nrow(R)), order.by = index(R))
  colnames(ret) <- name
  colnames(NAV) <- "NAV"
  delta_rel <- xts(matrix(0, nrow(w), ncol(w)), order.by = index(w))
  NAV[rebalance_indices[1]] <- 1  # initial NAV of 1$
  w_eop <- w[rebalance_indices[1], ]  # I don't want to count the initial huge turnover
  for (t in rebalance_indices[1]:nrow(R)) {
    if (t > rebalance_indices[1])
      NAV[t] <- NAV[t-1]*NAV_relchange  # just to keep track
    if (t %in% rebalance_indices) {
      delta_rel[t, ] <- w[t, ] - w_eop
      cash <- 1 - sum(w[t, ])       # normalized cash wrt NAV
      ret[t] <- sum(R[t, ]*w[t, ])  # recall w is normalized wrt NAV
      w_eop <- (1 + R[t, ])*w[t, ]  # new w but it is still normalized wrt previous NAV which is not the correct normalization
    }
    else {
      w[t, ] <- w_eop  # just to keep track
      cash <- 1 - sum(w_eop)       # normalized cash wrt NAV
      ret[t] <- sum(R[t, ]*w_eop)  # recall w is normalized wrt NAV
      w_eop <- (1 + R[t, ])*w_eop  # new w but it is still normalized wrt previous NAV which is not the correct normalization
    }
    NAV_relchange <- cash + sum(w_eop)       # NAV_relchange(t+1) = NAV(t+1)/NAV(t)
    w_eop <- as.vector(w_eop/NAV_relchange)  # now w_eop is normalized wrt the current NAV(t+1)
  }
  
  # compute ROT based on normalized dollars
  PnL <- diff(NAV); colnames(PnL) <- "PnL"
  PnL_rel <- PnL/lag.xts(NAV)
  turnover_rel <- xts(rowSums(abs(delta_rel)), index(delta_rel))
  sum_PnL_rel <- sum(PnL_rel, na.rm = TRUE)
  sum_turnover_rel <- sum(turnover_rel, na.rm = TRUE) * length(rebalance_indices)/(length(rebalance_indices)-1)  # to compensate for the removed fist turnover
  ROT_bips <- 1e4*sum_PnL_rel/sum_turnover_rel
  
  return(list(rets = ret[rebalance_indices[1]:nrow(ret), ],
              ROT_bips = ROT_bips))
}