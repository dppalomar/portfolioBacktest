library(backtestPortfolio)
library(xts)


load("R_buildignore/INDEX_2010.RData")
prices1 <- INDEX_2010$X
object.size(prices1)
save(prices1, file="data/prices1.RData")


save(test, file = "test_gzip.RData")
#save(test, file = "test_gzip.RData", compress = "gzip", compression_level = 9)
#saveRDS(test, file = "test_gzip.rds")
#devtools::use_data(test, overwrite = TRUE)



#object.size(data_2010_2011)
#save(data_2010_2011, file = "data_2010_2011.RData")
