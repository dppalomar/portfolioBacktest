#' @title Backtest multiple portfolios over multiple datasets of stock prices in a rolling-window basis
#'
#' @description Automated backtesting of multiple portfolios over multiple 
#' datasets of stock prices in a rolling-window fashion. 
#' Each portfolio design is easily defined as a
#' function that takes as input a window of the stock prices and outputs the 
#' portfolio weights. Multiple portfolios can be easily specified as a list 
#' of functions or as files in a folder. Multiple datasets can be conveniently 
#' obtained with the function \code{\link{financialDataResample}} that resamples
#' the data downloaded with the function \code{\link{stockDataDownload}}.
#' The results can be later assessed and arranged with tables and plots.
#' The backtesting can be highly time-consuming depending on the number of 
#' portfolios and datasets can be performed with parallel computation over
#' multiple cores. Errors in functions are properly catched and handled so
#' that the execution of the overal backtesting is not stopped (error messages
#' are stored for debugging purposes). See 
#' \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html}{vignette}
#' for a detailed explanation.
#'
#' @param portfolio_funs List of functions (can also be a single function), each of them taking as input 
#'                       a dataset containing a list of \code{xts} objects (following the format of each 
#'                       element of the argument \code{dataset_list}) properly windowed (following the
#'                       rolling-window approach) and returning the portfolio as a vector of normalized
#'                       weights. See 
#'                       \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html}{vignette}
#'                       for details.
#' @param dataset_list List of datasets, each containing a list of \code{xts} objects, as generated
#'                     by the function \code{\link{financialDataResample}}.
#' @param folder_path If \code{portfolio_funs} is not defined, this should contain the path to a folder 
#'                    containing the portfolio functions saved in files. See 
#'                    \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html}{vignette}
#'                    for details.
#' @param source_to_local Logical value indicating whether to source files to local environment (default is \code{TRUE}).
#'                        It might be dangerous to set it to \code{FALSE} as in such case the global environment may be changed.
#'                        We suggest only to allow \code{FALSE} when the code in the source files does not work when locally 
#'                        sourced, e.g., with some versions of package \code{CVXR}. In that case, we further recommend to set 
#'                        \code{paral_portfolios > 1} to avoid changing the global environment.
#' @param price_name Name of the \code{xts} column in each dataset that contains the prices to be used in the portfolio return 
#'                   computation (default is \code{"adjusted"}).
#' @param paral_portfolios Interger indicating number of portfolios to be evaluated in parallel (default is \code{1}),
#'                         see \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html#parallel-backtesting}{vignette-paralle-mode} for details.
#' @param paral_datasets Interger indicating number of datasets to be evaluated in parallel (default is \code{1}),
#'                        see \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html#parallel-backtesting}{vignette-paralle-mode} for details.
#' @param show_progress_bar Logical value indicating whether to show progress bar (default is \code{FALSE}). 
#' @param benchmarks String vector indicating the benchmark portfolios to be incorporated, currently supports:
#' \itemize{\item{\code{uniform} - the uniform portfolio, \eqn{w = [1/N, ..., 1/N]} with \eqn{N} be number of stocks}
#'          \item{\code{IVP} - the inverse-volatility portfolio, with weights be inversely proportional the standard deviation of returns.}
#'          \item{\code{index} - the market index, requires an \code{xts} named `index` in the datasets.}}
#' @param shortselling Logical value indicating whether shortselling is allowed or not 
#'                     (default is \code{TRUE}, so no control for shorselling in the backtesting).
#' @param leverage Amount of leverage as in \eqn{||w||_1 <= leverage} 
#'                 (default is \code{Inf}, so no control for leverage in the backtesting).
#' @param T_rolling_window Length of the lookback rolling window in periods (default is \code{252}).
#' @param optimize_every How often the portfolio is to be optimized in periods (default is \code{20}).
#' @param rebalance_every How often the portfolio is to be rebalanced in periods (default is \code{1}).
#' @param bars_per_year Number of bars/periods per year. By default it will be calculated automatically 
#'                      (e.g., for daily data there are 252 bars/periods per year).
#' @param execution String that can be either \code{"same period"} (default) or \code{"next period"}. 
#'                  At the rebalancing period \code{t}, the portfolio has used information up to (and including)
#'                  period \code{t}. Same period execution means one can get into the position at that period \code{t},
#'                  whereas the next period execution means that one can only get into the position the following period.
#' @param cost List containing four different types of transaction costs (common for all assets) 
#'             for buying, selling, shorting, and long leveraging. The default is 
#'             \code{cost = list(buy = 0e-4, sell = 0e-4, short = 0e-4, long_leverage = 0e-4)}.
#'             If some elements are not specified then they will be automatically set to zero.
#' @param cpu_time_limit Time limit for executing each portfolio function over a single data set 
#'                       (default is \code{Inf}, so no time limit).
#' @param return_portfolio Logical value indicating whether to return the portfolios (default is \code{TRUE}).
#'                         Two portfolios are returned: \code{w_designed} is the designed portfolio at each
#'                         given rebalancing period (using all the information up to and including that period,
#'                         which can be executed either on the same period or the following period)
#'                         and \code{w_bop} is the "beginning-of-period" portfolio (i.e., at each period it contains 
#'                         the weights held in the market in the previous period so that the portfolio return at 
#'                         that period is just the product of the asset returns and \code{w_bop} at that period.)
#' @param return_returns Logical value indicating whether to return the portfolio returns (default is \code{TRUE}).
#'                       Two series are returned: \code{return} with the portfolio returns and \code{wealth}
#'                       with the portfolio wealth (aka cumulative P&L).
#' 
#' @return List with the portfolio backtest results, see 
#'         \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html#result-format}{vignette-result-format}
#'         for details. It can be accessed directly, but we highly recommend the use of the package specific functions to extract 
#'         any required information, namely, \code{\link{backtestSelector}}, \code{\link{backtestTable}}, 
#'         \code{\link{backtestBoxPlot}}, \code{\link{backtestLeaderboard}},
#'         \code{\link{backtestSummary}}, \code{\link{summaryTable}}, \code{\link{summaryBarPlot}}.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{stockDataDownload}}, \code{\link{financialDataResample}},
#'          \code{\link{backtestSelector}}, \code{\link{backtestTable}}, 
#'          \code{\link{backtestBoxPlot}}, \code{\link{backtestLeaderboard}},
#'          \code{\link{backtestSummary}}, \code{\link{summaryTable}}, \code{\link{summaryBarPlot}}.
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
#' # check your result
#' names(bt)
#' backtestSelector(bt, portfolio_name = "Uniform", measures = c("Sharpe ratio", "max drawdown"))
#' backtestTable(bt, measures = c("Sharpe ratio", "max drawdown"))
#' bt_summary <- backtestSummary(bt)
#' summaryTable(bt_summary)
#' }
#' 
#' @import xts
#' @importFrom zoo index
#' @importFrom pbapply pblapply pboptions
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ
#' @importFrom utils object.size
#' @export
portfolioBacktest <- function(portfolio_funs = NULL, dataset_list, folder_path = NULL, source_to_local = TRUE, price_name = "adjusted",
                              paral_portfolios = 1, paral_datasets = 1,
                              show_progress_bar = FALSE, benchmarks = NULL, 
                              shortselling = TRUE, leverage = Inf,
                              T_rolling_window = 252, optimize_every = 20, rebalance_every = 1, bars_per_year = NA,
                              execution = c("same period", "next period"), 
                              cost = list(buy = 0e-4, sell = 0e-4, short = 0e-4, long_leverage = 0e-4),
                              cpu_time_limit = Inf,
                              return_portfolio = TRUE, return_returns = TRUE) {
  ####### error control ########
  paral_portfolios <- round(paral_portfolios)
  paral_datasets <- round(paral_datasets)
  if (paral_portfolios < 1 || paral_datasets < 1) stop("Parallel number must be a positive interger.")
  if (is.null(folder_path) && is.null(portfolio_funs)) stop("The \"folder_path\" and \"portfolio_fun_list\" cannot be both NULL.")
  if (!is.null(portfolio_funs) && !is.list(portfolio_funs)) portfolio_funs <- list(portfolio_funs)
  cost <- modifyList(list(buy = 0, sell = 0, short = 0, long_leverage = 0), cost)
  if (length(cost) != 4) stop("Problem in specifying the cost: the elements can only be buy, sell, short, and long_leverage.")
  if (is.xts(dataset_list[[1]])) stop("Each element of \"dataset_list\" must be a list of xts objects. Try to surround your passed \"dataset_list\" with list().")
  if (!(price_name %in% names(dataset_list[[1]]))) stop("Price data xts element \"", price_name, "\" does not exist in dataset_list.")
  if (!is.xts(dataset_list[[1]][[1]])) stop("prices have to be xts.")
  ##############################
  
  if (is.null(names(dataset_list)))
    names(dataset_list) <- paste0("data", 1:length(dataset_list))
  
  if (!show_progress_bar) {
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
  }

    
  # parse arguments for subsequent calls
  args <- list(dataset_list = dataset_list, price_name = price_name,
               paral_datasets = paral_datasets,
               shortselling = shortselling, leverage = leverage,
               T_rolling_window = T_rolling_window, optimize_every = optimize_every, rebalance_every = rebalance_every, 
               bars_per_year = bars_per_year, execution = execution, cost = cost,
               cpu_time_limit = cpu_time_limit, return_portfolio = return_portfolio, return_returns = return_returns)
  
  # when portfolio_funs is passed
  if (!is.null(portfolio_funs)) {
    if (is.null(names(portfolio_funs))) portfolio_names <- paste0("fun", 1:length(portfolio_funs))
    else portfolio_names <- names(portfolio_funs)
    if ("" %in% portfolio_names) stop("Each element of \"portfolio_funs\" must has a unique name.")
    if (length(portfolio_names) != length(unique(portfolio_names))) stop("\"portfolio_funs\" contains repeated names.")
    
    message(sprintf("Backtesting %d portfolios over %d datasets (periodicity = %s data)...", 
                    length(portfolio_names), length(dataset_list), periodicity(dataset_list[[1]][[1]])$scale))
    
    singlePortfolioBacktest_safe <- function(...) {
      initial_packages <- search()  # snapshot of initial packages
      res <- do.call(singlePortfolioBacktest, args = list(...))
      detachPackages(setdiff(search(), initial_packages))  # detach the newly loaded packages
      return(res)
    }
    
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(portfolio_funs)) {
        if (show_progress_bar)
          message("  Backtesting function ", format(portfolio_names[i], width = 15), " (", i, "/", length(portfolio_names), ")")
        result[[i]] <- do.call(singlePortfolioBacktest_safe, args = c(list(portfolio_fun = portfolio_funs[[i]]), args))
      }
    } else {
      cl <- makeCluster(paral_portfolios)
      # export global variables
      exports <- setdiff(ls(envir = .GlobalEnv), c("portfolio_fun", "dataset_list", "show_progress_bar"))
      exports <- exports[sapply(exports, function(x) object.size(get(x, envir = .GlobalEnv))) < 10 * 1024^2]  # remove large objects of more than 10 Mb
      clusterExport(cl = cl, varlist = exports, envir = .GlobalEnv)
      # export attached packages
      attached_packages <- .packages()
      clusterExport(cl = cl, varlist = "attached_packages", envir = environment())
      invisible(clusterEvalQ(cl = cl, expr = sapply(attached_packages, function(pkg) require(pkg, character.only = TRUE))))
      # apply!
      result <- pblapply(portfolio_funs, 
                         function(portfolio_fun) 
                           do.call(singlePortfolioBacktest_safe, args = c(list(portfolio_fun = portfolio_fun), args)), 
                         cl = cl)
      stopCluster(cl)
    }
  } else {  # when folder_path is passed
    files <- list.files(folder_path)
    portfolio_names <- gsub(".R", "", files)
    if (length(portfolio_names) == 0) 
      stop(sprintf("Could not find any .R files in folder: %s", folder_path))
    
    message(sprintf("Backtesting %d portfolios over %d datasets (periodicity = %s data)...", 
                    length(portfolio_names), length(dataset_list), periodicity(dataset_list[[1]][[1]])$scale))
    
    singlePortfolioBacktest_folder_safe <- function(...) {
      args <- list(...)
      initial_packages <- search()  # snapshot of initial packages
      
      error_message <- tryCatch({
        # load the file with portfolio_fun()
        suppressMessages(source(file.path(args$folder_path, args$file), local = args$source_to_local))
        # fix the arguments
        if (args$source_to_local && exists("portfolio_fun", envir = environment()) ||
            !args$source_to_local && exists("portfolio_fun", envir = parent.env(environment())))  # in case the file does not contain a valid portfolio_fun()
          args <- c(list(portfolio_fun = portfolio_fun), args)
        args$folder_path <- args$file <- args$source_to_local <- NULL  # remove these arguments
        # do backtest
        res <- do.call(singlePortfolioBacktest, args)
        NULL}, 
        # warning = function(w) return(ifelse(!is.null(w$message), w$message, "")),
        error   = function(e) return(ifelse(!is.null(e$message), e$message, ""))
      )
      detachPackages(setdiff(search(), initial_packages))  # detach the newly loaded packages
      if (!is.null(error_message)) 
        return(list(source_error_message = error_message))
      else
        return(res)
    }
    
    dataset_list__ <- args$dataset_list  # backup dataset_list in case of being masked by the sourced files
    if (paral_portfolios == 1) {
      result <- list()
      for (i in 1:length(files)) {
        if (show_progress_bar)
          message("\n Backtesting file ", format(portfolio_names[i], width = 15), " (", i, "/", length(portfolio_names), ")")
        result[[i]] <- do.call(singlePortfolioBacktest_folder_safe, args = c(list(folder_path = folder_path,
                                                                                  file = files[i],
                                                                                  source_to_local = source_to_local), args))
      }
    } else {
      cl <- makeCluster(paral_portfolios)
      
      result <- pblapply(files, 
                         function(file) 
                           do.call(singlePortfolioBacktest_folder_safe, args = c(list(folder_path = folder_path,
                                                                                      file = file,
                                                                                      source_to_local = source_to_local), args)), 
                         cl = cl)
      stopCluster(cl)
    }
  }
  names(result) <- portfolio_names
  attr(result, 'portfolio_index') <- 1:length(portfolio_names)

  
  #
  # add benchmarks if necessary
  #
  if (!is.null(benchmarks)) {
    message("\nBacktesting benchmarks...")
    benchmark_portfolios <- benchmark_library[names(benchmark_library) %in% benchmarks]
    res_benchmarks <- list()
    for (i in seq_along(benchmark_portfolios)) {
      if (show_progress_bar)
        message("  Backtesting benchmark ", format(benchmarks[i], width = 15), " (", i, "/", length(benchmarks), ")")
      res_benchmarks[[i]] <- do.call(singlePortfolioBacktest, args = c(list(portfolio_fun = benchmark_portfolios[[i]]), args))
    }
    names(res_benchmarks) <- names(benchmark_portfolios)
    
    # add index if needed
    if ("index" %in% benchmarks) {
      if (show_progress_bar) 
        message("  Backtesting index     ", format("", width = 15), " (", i+1, "/", length(benchmarks), ")")
      args$price_name <- "index"
      args$optimize_every <- 1e10
      args$rebalance_every <- 1e10
      res_benchmarks$index <- do.call(singlePortfolioBacktest, args = c(list(portfolio_fun = function(...) return(1)), args))
    }
    result <- c(result, res_benchmarks)
    attr(result, 'contain_benchmark') <- TRUE
    attr(result, 'benchmark_index') <- (1:length(result))[-c(1:length(portfolio_names))]
  } else
    attr(result, 'contain_benchmark') <- FALSE
  
  return(result)
}






singlePortfolioBacktest <- function(...) {
  # arrange arguments
  args <- list(...)
  paral_datasets <- args$paral_datasets
  dataset_list <- args$dataset_list
  args$paral_datasets <- args$dataset_list <- NULL
  
  # do backtests
  if (paral_datasets == 1)
    result <- pblapply(dataset_list, function(data) do.call(singlePortfolioSingleXTSBacktest, args = c(list(data = data), args)))
  else {
    cl <- makeCluster(paral_datasets)
    # export global variables
    exports <- setdiff(ls(envir = .GlobalEnv), c("portfolio_fun", "data"))
    exports <- exports[sapply(exports, function(x) object.size(get(x, envir = .GlobalEnv))) < 10 * 1024^2]  # remove large objects of more than 10 Mb
    clusterExport(cl = cl, varlist = exports, envir = .GlobalEnv)
    # export attached packages
    attached_packages <- .packages()
    clusterExport(cl = cl, varlist = "attached_packages", envir = environment())
    invisible(clusterEvalQ(cl = cl, expr = sapply(attached_packages, function(pkg) require(pkg, character.only = TRUE))))
    # apply!
    result <- pblapply(dataset_list, 
                       function(data) do.call(singlePortfolioSingleXTSBacktest, args = c(list(data = data), args)),
                       cl = cl)
    stopCluster(cl) 
  }

  attr(result, "params") <- attr(args$portfolio_fun, "param")  # in case portfolio_fun had params as attribute (for parameter tuning)
  return(result)
}




# Backtesting of one portfolio function on one single xts
#
#' @import xts
singlePortfolioSingleXTSBacktest <- function(portfolio_fun, data, price_name,
                                             shortselling, leverage,
                                             T_rolling_window, optimize_every, rebalance_every, bars_per_year,
                                             execution, cost,
                                             cpu_time_limit,
                                             return_portfolio, return_returns) {
  if (!price_name %in% names(data))
    stop("Failed to find price data with name \"", price_name, "\"" , " in given dataset_list.")
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
  #if (periodicity(prices)$scale != "daily") stop("This function only accepts daily data.")
  #if (tzone(prices) != Sys.timezone()) tzone(prices) <- Sys.timezone()
  if (tzone(prices) != "UTC") tzone(prices) <- "UTC"
  #################################
  
  # indices
  #rebalancing_indices <- endpoints(prices, on = "weeks")[which(endpoints(prices, on = "weeks") >= T_rolling_window)]
  optimize_indices  <- seq(from = T_rolling_window, to = T, by = optimize_every)
  rebalance_indices <- seq(from = T_rolling_window, to = T, by = rebalance_every)
  if (any(!(optimize_indices %in% rebalance_indices))) 
    stop("The reoptimization indices have to be a subset of the rebalancing indices.")
  

  # compute w
  cpu_time <- c()
  w <- xts(matrix(NA, length(rebalance_indices), N), order.by = index(prices)[rebalance_indices])
  colnames(w) <- gsub(".Adjusted", "", colnames(prices))
  for (i in seq_along(rebalance_indices)) {
    idx_prices <- rebalance_indices[i]
    
    # call porfolio function if necessary
    if (idx_prices %in% optimize_indices) {  # reoptimize
      data_window  <- lapply(data, function(x) x[(idx_prices-T_rolling_window+1):idx_prices, ])
      start_time <- proc.time()[3] 
      error_capture <- R.utils::withTimeout(expr = evaluate::try_capture_stack(w[i, ] <- do.call(portfolio_fun, list(data_window)), 
                                                                               environment()), 
                                            timeout = cpu_time_limit, onTimeout = "silent")
      cpu_time <- c(cpu_time, as.numeric(proc.time()[3] - start_time))
    } else  # just rebalance without reoptimizing
      w[i, ] <- w[i-1, ]
    
    # parse errors
    portf <- check_portfolio_errors(error_capture, w[i, ], shortselling, leverage)
    if (portf$error)
      break  # return immediately when error happens
  }
  
  
  # return results
  if (portf$error) {
    res <- list(performance = portfolioPerformance(rets = NA, ROT_bps = NA, bars_per_year = NA),
                cpu_time = NA, 
                error = TRUE, 
                error_message = portf$error_message)
  } else {
    # compute portfolio returns
    R_lin <- PerformanceAnalytics::CalculateReturns(prices)
    portf <- returnPortfolio(R = R_lin, weights = w, execution = execution, cost = cost)
    # sanity check:
    # res_check <- PerformanceAnalytics::Return.portfolio(R_lin, weights = w, verbose = TRUE)
    # all.equal(portf$rets, res_check$returns)
    # all.equal(portf$w_bop, res_check$BOP.Weight, check.attributes = FALSE)
    
    res <- list(performance = portfolioPerformance(rets = portf$rets, ROT_bps = portf$ROT_bps, bars_per_year),
                cpu_time = mean(cpu_time), 
                error = FALSE, 
                error_message = NA)    
    if (return_portfolio) {
      res$w_designed <- w
      res$w_bop <- portf$w_bop
    }
    if (return_returns) {
      res$return <- portf$rets
      res$wealth <- portf$wealth
    }
  }
  return(res)
}





check_portfolio_errors <- function(error_capture, w, shortselling, leverage) {
  res <- list(error = FALSE, error_message = NA)
  
  # 1) check code execution errors
  if (is.list(error_capture)) {
    res$error <- TRUE
    res$error_message <- error_capture$message
    error_stack <- list("at"    = deparse(error_capture$call), 
                        "stack" = paste(sapply(error_capture$calls[-1], deparse), collapse = "\n"))
    attr(res$error_message, "error_stack") <- error_stack
  } else {
    # 2) check NA portfolio
    if (anyNA(w)) {
      res$error <- TRUE
      res$error_message <- "Returned portfolio contains NA."
    } else {
      # 3) check constraints
      if (!shortselling && any(w < -1e-6)) {
        res$error <- TRUE
        res$error_message <- "No-shortselling constraint not satisfied."
      }
      if (sum(abs(w)) > leverage + 1e-6) {
        res$error <- TRUE
        res$error_message <- c(res$error_message, "Leverage constraint not satisfied.")
      }
    }
  }
  return(res)
}




#
# Computes the returns of a portfolio of several assets (ignoring transaction costs):
#   R: is an xts with the individual asset linear returns (not log returns)
#   weights: is an xts with the normalized dollar allocation (wrt NAV, typically with sum=1) where
#            - each row represents a rebalancing date (with portfolio computed with info up to and including that period)
#            - dates with no rows means: no rebalancing (note that the portfolio may then violate some margin constraints...)
#   rets: are the returns for each period at the close
#
#' @import xts
returnPortfolio <- function(R, weights, 
                            execution = c("same period", "next period"), 
                            cost = list(buy = 0*10^(-4), sell = 0*10^(-4), short = 0*10^(-4), long_leverage = 0*10^(-4)),
                            initial_cash = 1) {
  ######## error control  #########
  if (!is.xts(R) || !is.xts(weights)) stop("This function only accepts xts.")
  # if (periodicity(R)$scale != "daily") stop("This function only accepts daily data.")
  if (!all(index(weights) %in% index(R))) stop("Weight dates do not appear in the returns")
  if (ncol(R) != ncol(weights)) stop("Number of weights does not match the number of assets in the returns")
  if (anyNA(R[-1])) stop("Returns contain NAs")
  #if (tzone(R) != Sys.timezone()) stop("Timezone of data is not the same as local timezone.")
  #################################
  
  # transaction cost
  compute_tc <- !all(cost == 0)
  tc <- 0

  # fill in w with NA to match the dates of R and lag appropriately
  w <- R; w[] <- NA; colnames(w) <- colnames(weights)
  w[index(weights), ] <- weights
  w <- switch(match.arg(execution),  # w[t] used info up to (including) price[t]
              "same period" = lag.xts(w, 1),  # w[t] is (idealistically) executed at price[t], so will multiply return[t+1]
              "next period" = lag.xts(w, 2),  # w[t] is executed one period later at price[t+1], so will multiply return[t+2]
              stop("Execution method unknown"))
  after_rebalance_indices <- which(!is.na(w[, 1]))
  # loop    (NAV contains the NAV at the beginning of the period before the open (i.e., end of previous period), like w,
  #          whereas ret contains the returns at the end of the period, so lag(ret) equals (NAV - lag(NAV))/lag(NAV))
  NAV <- ret <- xts(rep(NA, nrow(R)), order.by = index(R))
  colnames(ret) <- "portfolio return"
  colnames(NAV) <- "NAV"
  delta_rel <- xts(matrix(0, nrow(w), ncol(w)), order.by = index(w))
  NAV[after_rebalance_indices[1]] <- initial_cash
  w_eop <- w[after_rebalance_indices[1], ]  # don't want to count the initial huge turnover
  for (t in after_rebalance_indices[1]:nrow(R)) {
    if (t > after_rebalance_indices[1])
      NAV[t] <- NAV[t-1]*NAV_relchange
    if (t %in% after_rebalance_indices) {
      delta_rel[t, ] <- w[t, ] - w_eop
      if (compute_tc)
        tc <- cost$buy*sum(pos(delta_rel[t, ])) + cost$sell*sum(pos(-delta_rel[t, ])) +       # trading cost
              cost$long_leverage*max(0, sum(pos(w[t, ])) - 1) + cost$short*sum(pos(-w[t, ]))  # borrowing cost
      cash <- 1 - sum(w[t, ]) - tc  # normalized cash wrt NAV[t]
      ret[t] <- sum(R[t, ]*w[t, ]) - tc  # recall w is normalized wrt NAV[t]
      w_eop <- (1 + R[t, ])*w[t, ]  # new w but it is still normalized wrt previous NAV which is not the correct normalization
    } else {
      w[t, ] <- w_eop  # and delta_rel[t, ] is (already) zero
      if (compute_tc)
        tc <- cost$long_leverage*max(0, sum(pos(w_eop)) - 1) + cost$short*sum(pos(-w_eop))  # borrowing cost (no trading cost here)
      cash <- 1 - sum(w_eop) - tc  # normalized cash wrt NAV[t]
      ret[t] <- sum(R[t, ]*w_eop) - tc
      w_eop <- (1 + R[t, ])*w_eop
    }
    NAV_relchange <- cash + sum(w_eop)       # NAV_relchange(t+1) = NAV(t+1)/NAV(t)
    if (NAV_relchange <= 0 && t < nrow(R)) {  # if bankruptcy
      NAV[(t+1):nrow(R)] <- as.numeric(NAV[t]*NAV_relchange)
      ret[(t+1):nrow(R)] <- 0
      break
    }
    w_eop <- as.vector(w_eop/NAV_relchange)  # now w_eop is normalized wrt the current NAV(t+1)
  }

  # prepare time series to return
  rets <- ret[after_rebalance_indices[1]:nrow(ret), ]
  wealth <- initial_cash*c(xts(1, index(w)[after_rebalance_indices[1] - 1]), cumprod(1 + rets))
  colnames(wealth) <- "portfolio wealth"
  # sanity check: all.equal(na.omit(lag(wealth)), na.omit(NAV), check.attributes = FALSE)
  # sanity check: PnL <- diff(NAV); PnL_rel <- PnL/lag.xts(NAV); all.equal(na.omit(lag(ret)), na.omit(PnL_rel), check.attributes = FALSE)

  # compute ROT based on normalized dollars
  sum_turnover_rel <- sum(abs(delta_rel))  # turnover only for indices: after_rebalance_indices[-1] (only when rebalancing except the first)
  sum_PnL_rel <- sum(ret[after_rebalance_indices[1]:(tail(after_rebalance_indices, 1)-1), ])  # returns only for indices: after_rebalance_indices[1] to after_rebalance_indices[end]-1
  ROT_bps <- 1e4*sum_PnL_rel/sum_turnover_rel

  return(list(rets = rets,
              wealth = wealth,
              ROT_bps = ROT_bps,
              w_bop = w[!is.na(w[, 1])]))
}

pos <- function(x)
  pmax(0, as.numeric(x))

#cost <- list(buy = 17*10^(-4), sell = 17*10^(-4), long = 1*10^(-4), short = 1*10^(-4))  # for HK
#cost <- list(buy = 1*10^(-4), sell = 1*10^(-4), long = 1*10^(-4), short = 1*10^(-4))  # for US
#cost <- list(buy = 15*10^(-4), sell = 15*10^(-4), long = 15*10^(-4), short = 15*10^(-4))  # for CH
