library(xts)
library(quantmod)

multipleSymbolsDownload <- function(stock_namelist, begin_date, end_date) {
  data_set <- list()
  count <- 1
  valid_stock_symbols <- c()
  for (stock_index in 1:length(stock_namelist)) {
    error <- FALSE
    tmp <- tryCatch(Ad(getSymbols(stock_namelist[stock_index], 
                                  from = begin_date, to = end_date, auto.assign = FALSE)),
                    error = function(e) {error <<- TRUE})
    if (error) cat("fail to download stock: ", stock_namelist[stock_index], "\n")
    else {
      symbol <- stock_namelist[stock_index]
      valid_stock_symbols <- c(valid_stock_symbols, symbol)
      data_set[[count]] <- tmp
      count <- count + 1
    }
  }
  names(data_set) <- valid_stock_symbols
  
  # assemble into one data set
  tmp <- multipleXTSMerge(data_set, "left")
  mask <- !(valid_stock_symbols %in% checkNAPattern(tmp))
  return(tmp[, mask])
}

multipleXTSMerge <- function(xts_list, join) {
  res <- xts_list[[1]]
  if (length(xts_list) == 1) return(res)
  
  for (i in 2:length(xts_list)) {
    res <- merge.xts(res, xts_list[[i]], join = join)
  }
  colnames(res) <- names(xts_list)
  return(res)
}

checkNAPattern <- function(prices) {
  prices <- as.matrix(prices)
  invalid_symbols <- c()
  for (i in 1:(nrow(prices)-1)) {
    mask1 <- is.na(prices[i, ])
    mask2 <- is.na(prices[i+1, ])
    if (any((mask2 - mask1) > 0)) {
      invalid_symbols <- c(invalid_symbols, colnames(prices)[(mask2 - mask1) > 0])
    }
  }
  return(unique(invalid_symbols))
}


# download data from Yahoo!Finance
load('data-raw/SP500_symbols.RData')
SP500_YAHOO <- multipleSymbolsDownload(stock_namelist = SP500_symbols, begin_date = "2008-12-01", end_date = "2018-12-01")
save(SP500_YAHOO, file = "data-raw/SP500_YAHOO.RData")

# download index
SP500_INDEX_YAHOO <- multipleSymbolsDownload(stock_namelist = "^GSPC", begin_date = "2008-12-01", end_date = "2018-12-01")
colnames(SP500_INDEX_YAHOO) <- "INDEX"
save(SP500_INDEX_YAHOO, file = "data-raw/SP500_INDEX_YAHOO.RData")

# save it in the variable dataset
genRandomSampleSingle <- function(source_dataset, N_sample, T_sample) {
  N <- ncol(source_dataset)
  T <- nrow(source_dataset)
  
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(source_dataset[t_start, ])
  mask <- rep(1:N)[mask]
  
  if (length(mask) <= N_sample)
    stock_mask <- mask
  else
    stock_mask <- sample(mask, N_sample)
  
  return(source_dataset[t_start:(t_start+T_sample-1), stock_mask])
}

dataset <- list()
N_sample <- 50
T_sample <- 252*2
for (i in 1:10) {
  # generate random segment from complete data set
  dataset_seg <- genRandomSampleSingle(source_dataset = SP500_YAHOO, N_sample = N_sample, T_sample = T_sample)
  # cut the corresponding segment from SP500 INDEX
  index_seg <- SP500_INDEX_YAHOO[index(dataset_seg), ]
  dataset[[i]] <- list(prices = dataset_seg, index = index_seg)
}

# check if row number matched
sapply(dataset, function(x){nrow(x$prices) == nrow(x$index)})
# check if any NA exists
sapply(dataset, function(x){anyNA(x$prices) || anyNA(x$index)})
# check if date index matched
sapply(dataset, function(x){all(index(x$prices) == index(x$index))})

# check data
for (i in 1:50) {
  print(plot(dataset[[2]]$prices[, i]))
  readline("press any key to continue")
}

# save dataset to the data folder
save(dataset, file = "data-raw/dataset.RData")
devtools::use_data(dataset, overwrite = TRUE)
