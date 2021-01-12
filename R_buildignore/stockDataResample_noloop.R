# # unfortunately, this function without loops is not faster...
# library(microbenchmark)
# SP500_YAHOO <- stockDataDownload(stock_symbols = SP500_symbols, from = "2010-12-01", to = "2018-12-01")
# op <- microbenchmark(
#   loop = financialDataResample(SP500_YAHOO, N = 50, T = 252*2, num_datasets = 100),
#   apply = financialDataResample_noloop(SP500_YAHOO, N = 50, T = 252*2, num_datasets = 100),
#   times = 100)
# print(op)
#
# set.seed(42)
# mydataset <- financialDataResample(SP500_YAHOO, N = 50, T = 252*2, num_datasets = 1000)
# set.seed(42)
# mydataset2 <- financialDataResample_noloop(SP500_YAHOO, N = 50, T = 252*2, num_datasets = 1000)
# identical(mydataset, mydataset2)
financialDataResample_noloop <- function(X, N_sample = 50, T_sample = 2*252, num_datasets = 10, rm_stocks_with_na = TRUE) {
  # check data time zone
  if ((!is.null(X$index)) && any(index(X$open) != index(X$index))) stop("The date indexes of \"X\" do not match.")
  
  # if required, remove stocks with non-leading missing data
  if (rm_stocks_with_na) {
    na_nonleading_mask <- apply(X$open, 2, function(x) {any(diff(is.na(x)) > 0)})
    if (any(na_nonleading_mask)) stop("\"X\" does not satisfy monotone missing-data pattern.")
  }
  
  # resampling
  cols <- sapply(X, ncol)
  N <- max(cols)  # some elements will have N cols, others just 1
  elems_N <- which(cols == N)
  elems_1 <- if (any(cols == 1)) which(cols == 1)
  else NULL
  T <- nrow(X[[1]])
  if (T < T_sample) stop("\"T_sample\" can not be greater than the date length of \"X\".")
  dataset <- lapply(1:num_datasets, FUN = function(i) {  # this is like: for (i in 1:num_datasets) {
    t_start <- sample(T-T_sample+1, 1)
    t_mask <- t_start:(t_start+T_sample-1)
    N_mask <- rep(1:N)[!is.na(X[[1]][t_start, ])]
    stock_mask <- if (length(N_mask) <= N_sample) N_mask
    else sample(N_mask, N_sample)
    dataset_i <- vector("list", length(X))
    names(dataset_i) <- names(X)
    dataset_i[elems_N] <- lapply(X[elems_N], function(x) {x[t_mask, stock_mask]})
    if (!is.null(elems_1)) dataset_i[elems_1] <- lapply(X[elems_1], function(x) {x[t_mask, ]})
    return(dataset_i)
  })
  names(dataset) <- paste("dataset", 1:num_datasets)
  return(dataset)
}

