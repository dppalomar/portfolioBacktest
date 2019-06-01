#' @title Backtesting Portfolio Design on a Rolling-Window Basis of Set of Prices
#'
#' @description Backtest a portfolio design contained in a function on a rolling-window basis of a set of prices.
#'
#' @param portfolio_funs function that takes as input an \code{xts} containing the stock prices and returns the portfolio weights.
#' @param dataset_list a list with each element be a list of follows:
#' \itemize{\item{\code{prices} - an xts object containing the stock prices for backtesting}
#'          \item{\code{index} - an xts object containing the market index of above \code{prices} with exact same time index.}}
#' @param folder_path the path of folder containing the portfolio functions saved in files, only valid when \code{portfolio_fun} is not passed.
#' @param paral_portfolios an positive interger indicating number of portfolios to be evaluated in parallel (default \code{1}).
#' @param paral_datasets an positive interger indicating number of datasets to be used per portfolio in parallel (default \code{1}).
#' @param show_progress_bar logical value indicating whether to show progress bar (default: \code{FALSE}). 
#' @param benchmark a string vector indicating the benchmark portfolios to be incorporated, now support the follows:
#' \itemize{\item{\code{uniform} - the uniform portfolio, \eqn{w = [1/N, ..., 1/N]} with \eqn{N} be number of stocks (default)}
#'          \item{\code{index} - the market index return, requires pass \code{index} in corresponding dataset}}
#' @param shortselling logical value indicating whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage) (default \code{1}).
#' @param T_rolling_window length of the rolling window (default \code{252}).
#' @param optimize_every how often the portfolio is to be optimized (default \code{20}).
#' @param rebalance_every how often the portfolio is to be rebalanced (default \code{20}).
#' @param cpu_time_limit time limit for executing portfolio function once on a single data set (default \code{Inf}).
#' @param return_portfolio logical value indicating whether to return portfolios (default \code{TRUE}).
#' @param return_return logical value indicating whether to return the portfolio returns (default \code{TRUE}).
#' 
#' @return A list containing the portfolio backtest results to be further handled by \code{backtestSelector()} and \code{backtestSummary()}.
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
#' @import xts
#' @export
#' 
portfolioBacktest <- function(portfolio_funs = NULL, dataset_list, folder_path = NULL, paral_portfolios = 1, 
                              show_progress_bar = FALSE, benchmark = NULL, ...) {
  ####### error control ########
  paral_portfolios <- round(paral_portfolios)
  paral_datasets <- list(...)$paral_datasets
  if (is.null(paral_datasets)) paral_datasets = 1
  paral_datasets <- round(paral_datasets)
  if (paral_portfolios < 1 || paral_datasets < 1) stop("Parallel number must be positive interger")
  if ((paral_portfolios > 1 || paral_datasets > 1) && !require(doSNOW, quietly = TRUE)) 
    stop("Package \"doSNOW\" needed for parallel mode. Please install it.")
  if (is.null(folder_path) && is.null(portfolio_funs)) stop("The \"folder_path\" and \"portfolio_fun_list\" can not both be NULL")
  if (!is.null(portfolio_funs) && !is.list(portfolio_funs)) portfolio_funs <- list(portfolio_funs)
  ##############################
  
  # when portfolio_funs is passed
  if (!is.null(portfolio_funs)) {
    if (is.null(names(portfolio_funs))) portfolio_names <- paste0("fun", 1:length(portfolio_funs))
    else portfolio_names <- names(portfolio_funs)
    
    # incase the extra packages are loaded
    safeEval <- function(portfolio_fun, dataset_list, show_progress_bar, ...) {
      packages_default <- search() # snap the default packages
      res <- singlePortfolioBacktest(portfolio_fun = portfolio_fun, dataset_list = dataset_list, show_progress_bar = show_progress_bar, ...)
      packages_now <- search()# detach the newly loaded packages
      packages_det <- packages_now[!(packages_now %in% packages_default)]
      detachPackages(packages_det)
      return(res)
    }
    
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(portfolio_funs)) {
        if (show_progress_bar) cat(sprintf("\n Backtesting function %s (%d/%d)\n", format(portfolio_names[i], width = 15), i, length(portfolio_names)))
        result[[i]] <- safeEval(portfolio_funs[[i]], dataset_list, show_progress_bar, ...)
      }
    } else {
      # creat the progress bar based on function number
      if (show_progress_bar) {
        sink(file = tempfile())
        pb <- txtProgressBar(max = length(portfolio_funs), style = 3)
        sink()
        opts <- list(progress = function(n) setTxtProgressBar(pb, n))
        cat(paste0("Evaluating overall ", length(portfolio_funs), " portfolio functions parallel \n"))
        opts$progress(0)
      } else opts <- list()
      
      cl <- makeCluster(paral_portfolios)
      registerDoSNOW(cl)
      exports <- ls(envir = .GlobalEnv)
      exports <- exports[! exports %in% c("portfolio_fun", "dataset_list", "show_progress_bar")]
      result <- foreach(portfolio_fun = portfolio_funs, .combine = c, .export = exports, 
                        .packages = .packages(), .options.snow = opts) %dopar% {
        return(list(safeEval(portfolio_fun, dataset_list, show_progress_bar, ...)))
      }
      if (show_progress_bar) close(pb)
      stopCluster(cl) 
    }
    
  } else { # when folder_path is passed
    files <- list.files(folder_path)
    portfolio_names <- gsub(".R", "", files)
    
    # define a safe enviroment to source .R files
    dataset_list__ <- dataset_list # backup dataset_list in case of being covered by sourced files
    safeEval <- function(folder_path, file, dataset_list__, show_progress_bar, ...) {
      packages_default <- search() # snap the default packages
      source_error <- FALSE; source_error_message <- NA
      tryCatch(expr    = {suppressMessages(source(paste0(folder_path, "/", file), local = TRUE))
                          res <- singlePortfolioBacktest(portfolio_fun = portfolio_fun, dataset_list = dataset_list__, 
                                                         show_progress_bar = show_progress_bar, ...)}, 
               warning = function(w){source_error <<- TRUE; source_error_message <<- w$message}, 
               error   = function(e){source_error <<- TRUE; source_error_message <<- e$message})
      packages_now <- search()# detach the newly loaded packages
      packages_det <- packages_now[!(packages_now %in% packages_default)]
      detachPackages(packages_det)
      if (source_error) return(list(source_error_message = source_error_message))
      return(res)
    }
    
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(files)) {
        if (show_progress_bar) cat(sprintf("\n Backtesting file %s (%d/%d)\n", format(portfolio_names[i], width = 15), i, length(portfolio_names)))
        result[[i]] <- safeEval(folder_path, files[i], dataset_list__, show_progress_bar, ...)
      }
    } else {
      # creat the progress bar based on files number
      if (show_progress_bar) {
        sink(file = tempfile())
        pb <- txtProgressBar(max = length(files), style = 3)
        sink()
        opts <- list(progress = function(n) setTxtProgressBar(pb, n))
        cat(paste0("Evaluating overall ", length(files), " portfolio functions (from file) parallel \n"))
        opts$progress(0)
      } else opts <- list()
      
      cl <- makeCluster(paral_portfolios)
      registerDoSNOW(cl)
      result <- foreach(file = files, .combine = c, .options.snow = opts) %dopar% {
        return(list(safeEval(folder_path, file, dataset_list__, show_progress_bar, ...)))
      }
      if (show_progress_bar) close(pb)
      stopCluster(cl) 
    }
  }
  
  names(result) <- portfolio_names
  
  # add benchmark and return result
  res_benchmark <- benchmarkBacktest(dataset_list = dataset_list, benchmark = benchmark, show_progress_bar = show_progress_bar, ...)
  result <- c(result, res_benchmark)
  attr(result, 'portfolio_index') <- 1:length(portfolio_names)
  attr(result, 'contain_benchmark') <- length(res_benchmark) > 0
  attr(result, 'benchmark_index') <- (1:length(result))[-c(1:length(portfolio_names))]
  return(result)
}

# benchmark portfolio backtest

benchmarkBacktest <- function(dataset_list, benchmark, show_progress_bar, ...) {
  
  res <- list()
  if ("uniform" %in% benchmark) {
    if (show_progress_bar) cat("\n Evaluating benchmark-uniform\n")
    res$uniform <- singlePortfolioBacktest(portfolio_fun = uniform_portfolio_fun, dataset_list = dataset_list, show_progress_bar = show_progress_bar, ...)
  }
  if ("index" %in% benchmark) {
    if (show_progress_bar) cat("\n Evaluating benchmark-index\n")
    res$index <- singlePortfolioBacktest(portfolio_fun = NULL, dataset_list = dataset_list, show_progress_bar = show_progress_bar, market = TRUE, ...)
  }
  return(res)
}

singlePortfolioBacktest <- function(portfolio_fun, dataset_list, show_progress_bar, paral_datasets = 1, ...) {
  
  paral_datasets <- round(paral_datasets)
  
  # creat the progress bar
  if (show_progress_bar) {
    sink(file = tempfile())
    pb <- txtProgressBar(max = length(dataset_list), style = 3)
    sink()
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    opts$progress(0)
  } else opts <- list()
  
  # when price is a list of xts object
  if (paral_datasets == 1) { ########## no-parallel mode
    result <- list()
    for (i in 1:length(dataset_list)) {
      result[[i]] <- singlePortfolioSingleXTSBacktest(portfolio_fun = portfolio_fun, data = dataset_list[[i]], ...)
      if (show_progress_bar) opts$progress(i) # show progress bar
    }
  } else {               ########### parallel mode
    cl <- makeCluster(paral_datasets)
    registerDoSNOW(cl)
    exports <- ls(envir = .GlobalEnv)
    exports <- exports[! exports %in% c("portfolio_fun", "dat")]
    result <- foreach(dat = dataset_list, .combine = c, .packages = .packages(), .export = ls(envir = .GlobalEnv), .options.snow = opts) %dopar% {
      return(list(singlePortfolioSingleXTSBacktest(portfolio_fun = portfolio_fun, data = dat, ...)))
    }
    stopCluster(cl) 
  }
  
  if (is.null(names(dataset_list)))
    names(result) <- paste0("data", 1:length(dataset_list))
  else
    names(result) <- names(dataset_list)
  
  return(result)
}


# singlePortfolioSingleDataBacktest <- function(portfolio_fun, benchmark = c(), dat, ...) {
#   res <- singlePortfolioSingleXTSBacktest(portfolio_fun = portfolio_fun, prices = dat$prices, ...)
#   if ("uniform" %in% benchmark) res$benchmark$uniform <- singlePortfolioSingleXTSBacktest(portfolio_fun = uniform_portfolio_fun, prices = dat$prices, ...)
#   if ("index" %in% benchmark) res$benchmark$index <- singlePortfolioSingleXTSBacktest(market = dat$index, ...)
#   return(res)
# }


# Backtesting of one portfolio function on one single xts
#
#' @import xts
singlePortfolioSingleXTSBacktest <- function(portfolio_fun, data, price_name = "adjusted", market = FALSE,
                                             return_portfolio = TRUE, return_return = TRUE,
                                             shortselling = TRUE, leverage = Inf, cpu_time_limit = Inf,
                                             T_rolling_window = 252, optimize_every = 20, rebalance_every = 1) {
  # creat return container
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
    if (return_return) {res$return <- idx_return; res$cumPnL <- idx_prices_window[-1]/as.numeric(idx_prices_window[1])}
    return(res)
  }
  
  if (!price_name %in% names(data)) 
    stop(paste0("fail to find price data with name \"", price_name, "\"" , " in given dataset_list"))
  prices <- data[[price_name]]
  
  ######## error control  #########
  if (is.list(prices)) stop("prices have to be xts, not a list, make sure you index the list with double brackets [[.]]")
  if (!is.xts(prices)) stop("prices have to be xts")
  N <- ncol(prices)
  T <- nrow(prices)
  if (T_rolling_window >= T) stop("T is not large enough for the given sliding window length")
  if (optimize_every%%rebalance_every != 0) stop("The reoptimization period has to be a multiple of the rebalancing period")
  if (anyNA(prices)) stop("prices contain NAs")
  if (!is.function(portfolio_fun)) stop("portfolio_fun is not a function")
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
      data_window  <- lapply(data, function(x){x[(idx_prices-T_rolling_window+1):idx_prices, ]})
      start_time <- proc.time()[3] 
      error_capture <- R.utils::withTimeout(expr = evaluate::try_capture_stack(w[i, ] <- do.call(portfolio_fun, list(data_window)), environment()), 
                                            timeout = cpu_time_limit, onTimeout = "silent")
      cpu_time <- c(cpu_time, as.numeric(proc.time()[3] - start_time))
    } else {# just rebalance without reoptimizing
      w[i, ] <- w[i-1, ]
    }
    
    # check if error happens
    if (is.list(error_capture)) {
      error <- TRUE
      error_message <- error_capture$message
      error_stack <- list("at" = deparse(error_capture$call), 
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
  if (return_return) {res$return <- rets; res$cumPnL <- cumprod(1 + rets)}
  
  return(res)
}

# analyse the performance of a portfolio
#   rets: is an xts recording portfolio's return
#   weights: is an xts with the normalized dollar allocation (wrt NAV, typically with sum=1) where
#            - each row represents a rebalancing date (with portfolio computed with info up to and including that day)
#            - dates with no rows means no rebalancing (note that the portfolio may then violate some margin constraints...)
#
portfolioPerformance <- function(rets = NA, ROT_bips = NA) {
  performance <- rep(NA, 7)
  names(performance) <- c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", 
                          "Sterling ratio", "Omega ratio", "ROT bps")
  # return blank vector if no need computation
  if (anyNA(rets)) return(performance)
  
  # fill the elements one by one
  performance['Sharpe ratio']      <- PerformanceAnalytics::SharpeRatio.annualized(rets)
  performance['max drawdown']      <- PerformanceAnalytics::maxDrawdown(rets)
  performance['annual return']     <- PerformanceAnalytics::Return.annualized(rets)
  performance['annual volatility'] <- PerformanceAnalytics::StdDev.annualized(rets)
  performance['Sterling ratio']    <- PerformanceAnalytics::Return.annualized(rets) / PerformanceAnalytics::maxDrawdown(rets)
  performance['Omega ratio']       <- PerformanceAnalytics::Omega(rets)
  performance['ROT bps']           <- ROT_bips
  
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
         "same day" = { w <- lag(w) },
         "next day" = { w <- lag(w, 2) },
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
  PnL_rel <- PnL/lag(NAV)
  turnover_rel <- xts(rowSums(abs(delta_rel)), index(delta_rel))
  sum_PnL_rel <- sum(PnL_rel, na.rm = TRUE)
  sum_turnover_rel <- sum(turnover_rel, na.rm = TRUE) * length(rebalance_indices)/(length(rebalance_indices)-1)  # to compensate for the removed fist turnover
  ROT_bips <- 1e4*sum_PnL_rel/sum_turnover_rel
  
  return(list(rets = ret[rebalance_indices[1]:nrow(ret), ],
              ROT_bips = ROT_bips))
}