library(backtestPortfolio)
library(xts)

# dimensions of samples
N_sample <- 50
T_sample <- 2*252  #2 years of data

prices <- list()

load("data_private/stocks_SP500_1999_2005.RData")
prices_ <- exp(cumsum(data_1999_2005$X))
N <- ncol(prices_)
T <- nrow(prices_)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  prices <- c(prices, list(prices_[t_start:(t_start+T_sample-1), sample(N, 50)]))
}

load("data_private/stocks_SP500_2006_2012.RData")
prices_ <- exp(cumsum(data_2006_2012$X))
N <- ncol(prices_)
T <- nrow(prices_)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  prices <- c(prices, list(prices_[t_start:(t_start+T_sample-1), sample(N, 50)]))
}

load("data_private/stocks_SP500_2010_2015.RData")
prices_ <- exp(cumsum(data_2010_2015$X))
N <- ncol(prices_)
T <- nrow(prices_)
for(i in 1:10) {
  t_start <- sample(T-T_sample+1, 1)
  prices <- c(prices, list(prices_[t_start:(t_start+T_sample-1), sample(N, 50)]))
}

object.size(prices)
save(prices, file="data/prices.RData")



#save(test, file = "test_gzip.RData")
#save(test, file = "test_gzip.RData", compress = "gzip", compression_level = 9)
#saveRDS(test, file = "test_gzip.rds")
#devtools::use_data(test, overwrite = TRUE)



#object.size(data_2010_2011)
#save(data_2010_2011, file = "data_2010_2011.RData")
