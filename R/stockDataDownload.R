#' @title Stocks data download from Internet
#'
#' @description Use package "quantmod" to download stocks prices data.
#' 
#' @param stock_symbols a string vector containing symbols of stocks to be downloaded.
#'                      User can pass the market index symbol as its attribute "index_symbol" 
#'                      (only considered when argument `index_symbol` is not passed).
#' @param index_symbol a string as the market index symbol of stocks included in `stock_symbols`. 
#' @param check_monotone a logical value indicating whether to check and delete data not satisfying monotone missing pattern.
#' @param from a string as the starting date, e.g., "2017-08-17".
#' @param to a string as the ending date (not included), e.g., "2017-09-17".
#' @param ... additional arguments to be passed to `quantmod::getSymbols()``
#'
#' @return a list of 7 xts objects which are `open`, `high`, `low`, `close`, `volume`, `adjusted` and `index`.
#'         Note that `index` will only be returned when correct index symbols is passed.
#' 
#' @author Rui Zhou and Daniel P. Palomar
#' 
#' @examples
#' \dontrun{
#' library(portfolioBacktest)
#' data("SP500_symbols")
#' 
#' # download data from internet
#' SP500 <- stockDataDownload(stock_symbols = SP500_symbols,
#'                            from = "2008-12-01", to = "2018-12-01")
#' }
#' 
#' @import xts 
#'         quantmod
#' @export
stockDataDownload <- function(stock_symbols, index_symbol = NULL, check_monotone = TRUE, from, to, ...) {
  
  if (!require(quantmod, quietly = TRUE)) 
    stop("Package \"quantmod\" needed for this function to work. Please install it.")
  
  open <- high <- low <- close <- volume <- adjusted <- list()
  n_stocks <- length(stock_symbols)
  valid_count <- 0
  cat("Start downloading", n_stocks, "stocks...\n")
  
  sink(file = tempfile())
  pb <- txtProgressBar(max = n_stocks, style = 3)
  sink()
  
  stocks_fail <- c()
  for (i in 1:n_stocks) {
    fail_download <- FALSE
    tmp <- tryCatch(suppressWarnings(getSymbols(stock_symbols[i], from = from, to = to, auto.assign = FALSE, warnings = FALSE, ...)),
                    error = function(e) {fail_download <<- TRUE})
    if (fail_download) 
      stocks_fail <- c(stocks_fail, stock_symbols[i])
    else {
      valid_count <- valid_count + 1
      open[[valid_count]]     <- tmp[, 1]
      high[[valid_count]]     <- tmp[, 2]
      low[[valid_count]]      <- tmp[, 3]
      close[[valid_count]]    <- tmp[, 4]
      volume[[valid_count]]   <- tmp[, 5]
      adjusted[[valid_count]] <- tmp[, 6]
    }
    setTxtProgressBar(pb, i)
  }
  
  # show download information
  cat("\nSuccess:", valid_count, "\t fail:", n_stocks - valid_count, "\n")
  if (valid_count == 0) stop("Fail to download all stocks' data.")
  if (valid_count < n_stocks) cat("Fail to download:", stocks_fail, "\n")
  
  rt <- list("open"     = multipleXTSMerge(open),
             "high"     = multipleXTSMerge(high),
             "low"      = multipleXTSMerge(low),
             "close"    = multipleXTSMerge(close),
             "volume"   = multipleXTSMerge(volume),
             "adjusted" = multipleXTSMerge(adjusted))
  
  # remove non-monotone missing data
  monotone_mask <- apply(rt$open, 2, function(x){any(diff(is.na(x)) > 0)})
  if (check_monotone) rt <- lapply(rt, function(x){x[, !monotone_mask]})
  
  # also download index data
  if (is.null(index_symbol)) index_symbol <- attr(stock_symbols, "index_symbol")
  if (!is.null(index_symbol)) 
    rt$index <- tryCatch(Ad(suppressWarnings(getSymbols(index_symbol, from = from, to = to, auto.assign = FALSE, ...))),
                         error = function(e) {cat("Fail to download index \"", index_symbol, "\"\n", sep = "")})
  
  
  return(rt)
}

multipleXTSMerge <- function(xts_list) {
  res <- xts_list[[1]]
  if (length(xts_list) == 1) return(res)
  for (i in 2:length(xts_list))
    res <- merge.xts(res, xts_list[[i]], join = "outer")
  return(res)
}

#' @title Generate random resamples from the passed datasets
#' 
#' @param X a list of xts objects matching the return of function `stockDataDownload()`.
#' @param N_sample the number of stocks in each sample.
#' @param T_sample the length of observation.
#' @param num_datasets the number of wanted data sets.
#' @param check_monotone a logical value indicating whether to check the monotone missing pattern of `X`.
#' 
#' @return a list of subsets randomly resampled from `X`.
#' 
#' @author Rui ZHOU & Daniel P. Palomar
#' 
#' @examples 
#' \dontrun{
#' library(portfolioBacktest)
#' data("SP500_symbols")
#' 
#' # download data from internet
#' SP500 <- stockDataDownload(stock_symbols = SP500_symbols,
#'                            from = "2008-12-01", to = "2018-12-01")
#' # resample from downloaded, each with 50 stcoks and 2-year continuous data
#' my_dataset_list <- stockDataResample(SP500, N = 50, T = 252*2, num_datasets = 10)
#' }
#' 
#' @import xts
#' @export
stockDataResample <- function(X, N_sample = 50, T_sample = 252*2, num_datasets = 10, check_monotone = TRUE) {
  # check data time zone
  if (any(index(X$open) != index(X$index))) stop("The date indexes of X are not matched.")
  # check if the data is Monotone Missing
  monotone_mask <- apply(X$open, 2, function(x){any(diff(is.na(x)) > 0)})
  if (check_monotone && any(monotone_mask)) stop("X does not satisfy monotone missing-data pattern.")
  N <- ncol(X[[1]])
  T <- nrow(X[[1]])
  dataset <- list()
  for (i in 1:num_datasets) {
    if (T <= T_sample) {
      t_start <- 1
      t_mask <- 1:T
    }
    else {
      t_start <- sample(T-T_sample+1, 1)
      t_mask <- t_start:(t_start+T_sample-1)
    }
    
    mask <- rep(1:N)[!is.na(X[[1]][t_start, ])]
    if (length(mask) <= N_sample)
      stock_mask <- mask
    else
      stock_mask <- sample(mask, N_sample)
    dataset[[i]] <- lapply(X[1:6], function(x){x[t_mask, stock_mask]})
    dataset[[i]]$index <- X$index[t_mask, ]
  }
  return(dataset)
}
