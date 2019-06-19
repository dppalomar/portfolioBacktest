#' @title Download stock data from the Internet.
#'
#' @description This function is basically a robust wrapper for 
#' \code{\link[quantmod:getSymbols]{quantmod:getSymbols}} to download stock 
#' data from the internet. It will return 6 \code{xts} objects of the same 
#' dimensions named `open`, `high`, `low`, `close`, `volume`, `adjusted` 
#' and `index`. Additionally, it can return an \code{xts} object with an 
#' index. If the download for some stock fails after a few attempts they 
#' will be ignored and reported. Also, stocks with missing values can be 
#' optionally removed.
#' 
#' @param stock_symbols String vector containing the symbols of the stocks to be downloaded.
#'                      User can pass the market index symbol as its attribute `index_symbol`` 
#'                      (only considered when argument `index_symbol` is not passed).
#' @param index_symbol String of the market index symbol. 
#' @param from String as the starting date, e.g., "2017-08-17".
#' @param to String as the ending date (not included), e.g., "2017-09-17".
#' @param rm_stocks_with_na Logical value indicating whether to remove stocks with missing values 
#'                          (ignoring leading missing values). Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link[quantmod:getSymbols]{quantmod:getSymbols}}.
#'
#' @return List of 7 \code{xts} objects named `open`, `high`, `low`, `close`, `volume`, 
#'         `adjusted` and `index`. Note that `index` will only be returned when correct index symbols is passed.
#' 
#' @author Rui Zhou and Daniel P. Palomar
#' 
#' @seealso \code{\link{stockDataResample}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(SP500_symbols)
#' 
#' # download data from internet
#' SP500_data <- stockDataDownload(stock_symbols = SP500_symbols, 
#'                                 from = "2008-12-01", to = "2009-12-01")
#' }
#' 
#' @import xts 
#'         quantmod
#' @export
stockDataDownload <- function(stock_symbols, index_symbol = NULL, from, to, rm_stocks_with_na = TRUE, ...) {
  open <- high <- low <- close <- volume <- adjusted <- list()
  n_stocks <- length(stock_symbols)
  message("Downloading ", n_stocks, " stocks...")
  
  sink(file = tempfile())
  pb <- utils::txtProgressBar(max = n_stocks, style = 3)
  sink()

  valid_count <- 0
  stocks_fail <- c()
  for (i in 1:n_stocks) {
    tmp <- tryCatch(suppressWarnings(getSymbols(stock_symbols[i], from = from, to = to, auto.assign = FALSE, warnings = FALSE, ...)),
                    error = function(e) return(NULL))
    if (!is.xts(tmp) || !is.OHLCV(tmp) || ncol(tmp) != 6) 
      stocks_fail <- c(stocks_fail, stock_symbols[i])
    else {
      valid_count <- valid_count + 1
      open[[valid_count]]     <- tmp[, 1]  #Op(tmp)
      high[[valid_count]]     <- tmp[, 2]  #Hi(tmp)
      low[[valid_count]]      <- tmp[, 3]  #Lo(tmp)  <- this fails is the name of the stock is "LOW", it's a bug in quantmod
      close[[valid_count]]    <- tmp[, 4]  #Cl(tmp)
      volume[[valid_count]]   <- tmp[, 5]  #Vo(tmp)
      adjusted[[valid_count]] <- tmp[, 6]  #Ad(tmp)
    }
    utils::setTxtProgressBar(pb, i)
  }

  # show download information
  message("\nSuccesses: ", valid_count, "\t fails: ", n_stocks - valid_count, "\n")
  if (valid_count == 0) stop("Failed to download data from any stock.", call. = FALSE)
  if (valid_count < n_stocks) message("Failed to download: ", stocks_fail, "\n")
  
  rt <- list("open"     = multipleXTSMerge(open),
             "high"     = multipleXTSMerge(high),
             "low"      = multipleXTSMerge(low),
             "close"    = multipleXTSMerge(close),
             "volume"   = multipleXTSMerge(volume),
             "adjusted" = multipleXTSMerge(adjusted))
  
  # if required, remove stocks with non-leading missing data
  if (rm_stocks_with_na) {
    na_nonleading_mask <- apply(rt$adjusted, 2, function(x) {any(diff(is.na(x)) > 0)})
    rt <- lapply(rt, function(x) {x[, !na_nonleading_mask]})
  }
  
  # also download index data
  if (is.null(index_symbol)) index_symbol <- attr(stock_symbols, "index_symbol")  
  if (!is.null(index_symbol)) {
    message("Downloading index ", index_symbol, "...\n")
    rt$index <- tryCatch(Ad(suppressWarnings(getSymbols(index_symbol, from = from, to = to, auto.assign = FALSE, ...))),
                         error = function(e) {stop("Failed to download index \"", index_symbol, "\"\n", call. = FALSE)})
    # check if the date of stock prices and market index match
    if (any(index(rt$adjusted) != index(rt$index)))
      stop("Date of stocks prices and market index do not match.", call. = FALSE)
  }
  
  return(rt)
}

multipleXTSMerge <- function(xts_list) {
  res <- xts_list[[1]]
  if (length(xts_list) == 1) return(res)
  for (i in 2:length(xts_list))
    res <- xts::merge.xts(res, xts_list[[i]], join = "outer")
  return(res)
}



#' @title Generate random resamples from stock data.
#' 
#' @description This function resamples the stock data downloaded by
#' \code{\link{stockDataDownload}} to obtain many datasets for a 
#' subsequent backtesting with \code{\link{portfolioBacktest}}.
#' Given the original data, each resample is obtained by randomly
#' choosing a subset of the stock names and randomly choosing a
#' time period over the available long period.
#' 
#' @param X List of \code{xts} objects matching the structure returned by the function \code{\link{stockDataDownload}}.
#' @param N_sample Number of stocks in each resample.
#' @param T_sample Length of each resample (consecutive samples with a random initial time).
#' @param num_datasets Number of resampled datasets (chosen randomly among the stock universe).
#' @param check_stocks_with_na Logical value indicating whether to quit in case of stocks with missing 
#'                             values (ignoring leading missing values). Default is \code{TRUE}.
#' 
#' @return List of datasets resampled from \code{X}.
#' 
#' @author Rui Zhou and Daniel P. Palomar
#' 
#' @seealso \code{\link{stockDataDownload}}, \code{\link{portfolioBacktest}}
#' 
#' @examples 
#' \donttest{
#' library(portfolioBacktest)
#' data(SP500_symbols)
#' 
#' # download data from internet
#' SP500_data <- stockDataDownload(stock_symbols = SP500_symbols, 
#'                                 from = "2008-12-01", to = "2011-12-01")
#' # resample from downloaded, each with 50 stocks and 2-year continuous data
#' my_dataset_list <- stockDataResample(SP500_data, N = 50, T = 2*252, num_datasets = 10)
#' }
#' 
#' @import xts
#'         zoo
#' @export
stockDataResample <- function(X, N_sample = 50, T_sample = 2*252, num_datasets = 10, check_stocks_with_na = TRUE) {
  # check data time zone
  if (any(index(X$open) != index(X$index))) stop("The date indexes of \"X\" are not matched.")
  
  # if required, remove stocks with non-leading missing data
  if (check_stocks_with_na) {
    na_nonleading_mask <- apply(X$open, 2, function(x) {any(diff(is.na(x)) > 0)})
    if (any(na_nonleading_mask)) stop("\"X\" does not satisfy monotone missing-data pattern.")
  }
  
  N <- ncol(X[[1]])
  T <- nrow(X[[1]])
  if (T < T_sample) stop("\"T_sample\" can not be greater than the date length of \"X\".")
  dataset <- list()
  for (i in 1:num_datasets) {
    
    t_start <- sample(T-T_sample+1, 1)
    t_mask <- t_start:(t_start+T_sample-1)
    
    mask <- rep(1:N)[!is.na(X[[1]][t_start, ])]
    if (length(mask) <= N_sample)
      stock_mask <- mask
    else
      stock_mask <- sample(mask, N_sample)
    dataset[[i]] <- lapply(X[1:6], function(x){x[t_mask, stock_mask]})
    dataset[[i]]$index <- X$index[t_mask, ]
  }
  names(dataset) <- paste("dataset", 1:num_datasets)
  return(dataset)
}
