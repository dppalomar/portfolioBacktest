# dimensions of samples
N_sample <- 50
T_sample <- 2*252  #2 years of data

prices <- list()

load("data_private/stock_SP500_2008_2018.RData")
N <- ncol(stock_SP500_2008_2018)
T <- nrow(stock_SP500_2008_2018)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(stock_SP500_2008_2018[t_start, ])
  mask <- rep(1:N)[mask]
  if (length(mask) <= N_sample) {
    stock_mask <- mask
  } else {
    stock_mask <- sample(mask, N_sample)
  }
  prices <- c(prices, list(stock_SP500_2008_2018[t_start:(t_start+T_sample-1), stock_mask]))
}


load("data_private/stock_HSI50_2008_2018.RData")
N <- ncol(stock_HSI50_2008_2018)
T <- nrow(stock_HSI50_2008_2018)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(stock_HSI50_2008_2018[t_start, ])
  mask <- rep(1:N)[mask]
  if (length(mask) <= N_sample) {
    stock_mask <- mask
  } else {
    stock_mask <- sample(mask, N_sample)
  }
  prices <- c(prices, list(stock_HSI50_2008_2018[t_start:(t_start+T_sample-1), stock_mask]))
}

load("data_private/stock_NKY225_2008_2018.RData")
N <- ncol(stock_NKY225_2008_2018)
T <- nrow(stock_NKY225_2008_2018)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(stock_NKY225_2008_2018[t_start, ])
  mask <- rep(1:N)[mask]
  if (length(mask) <= N_sample) {
    stock_mask <- mask
  } else {
    stock_mask <- sample(mask, N_sample)
  }
  prices <- c(prices, list(stock_NKY225_2008_2018[t_start:(t_start+T_sample-1), stock_mask]))
}


load("data_private/stock_SHZ300_2008_2018.RData")
N <- ncol(stock_SHZ300_2008_2018)
T <- nrow(stock_SHZ300_2008_2018)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(stock_SHZ300_2008_2018[t_start, ])
  mask <- rep(1:N)[mask]
  if (length(mask) <= N_sample) {
    stock_mask <- mask
  } else {
    stock_mask <- sample(mask, N_sample)
  }
  prices <- c(prices, list(stock_SHZ300_2008_2018[t_start:(t_start+T_sample-1), stock_mask]))
}

load("data_private/stock_UKC101_2008_2018.RData")
N <- ncol(stock_UKC101_2008_2018)
T <- nrow(stock_UKC101_2008_2018)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  mask <- !is.na(stock_UKC101_2008_2018[t_start, ])
  mask <- rep(1:N)[mask]
  if (length(mask) <= N_sample) {
    stock_mask <- mask
  } else {
    stock_mask <- sample(mask, N_sample)
  }
  prices <- c(prices, list(stock_UKC101_2008_2018[t_start:(t_start+T_sample-1), stock_mask]))
}

object.size(prices)
save(prices, file="data/prices.RData")
