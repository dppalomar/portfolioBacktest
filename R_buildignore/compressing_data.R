library(portfolioBacktest)

load("data/SP500_symbols.RData")
object.size(SP500_symbols)
#save(SP500_symbols, file = "data/SP500_symbols_gzip.RData", compress = "gzip", compression_level = 9)
#saveRDS(SP500_symbols, file = "data/SP500_symbols_gzip.rds")


load("data/dataset.RData")
object.size(dataset)
object.size(dataset$`dataset 1`)
object.size(dataset$`dataset 1`$open)
#save(dataset, file = "data/dataset_gzip.RData", compress = "gzip", compression_level = 9)
#saveRDS(dataset, file = "data/dataset_gzip.rds")


for (i in seq_along(dataset))
  dataset[[i]] <- dataset[[i]][c("adjusted", "index")]
object.size(dataset)
save(dataset, file = "data/dataset.RData")





#devtools::use_data(test, overwrite = TRUE)
