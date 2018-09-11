library(backtestPortfolio)
library(xts)

load("data_private/stocks_SP500_2010.RData")
N <- ncol(INDEX_2010$X)
prices1 <- INDEX_2010$X[, sample(N, 50)]
prices2 <- INDEX_2010$X[, sample(N, 50)]
prices3 <- INDEX_2010$X[, sample(N, 50)]
prices4 <- INDEX_2010$X[, sample(N, 50)]
prices5 <- INDEX_2010$X[, sample(N, 50)]
object.size(prices1)
save(prices1, file="data/prices1.RData")
save(prices2, file="data/prices2.RData")
save(prices3, file="data/prices3.RData")
save(prices4, file="data/prices4.RData")
save(prices5, file="data/prices5.RData")



#save(test, file = "test_gzip.RData")
#save(test, file = "test_gzip.RData", compress = "gzip", compression_level = 9)
#saveRDS(test, file = "test_gzip.rds")
#devtools::use_data(test, overwrite = TRUE)



#object.size(data_2010_2011)
#save(data_2010_2011, file = "data_2010_2011.RData")
