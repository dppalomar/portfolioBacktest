library(portfolioBacktest)

data("SP500_symbols")

SP500_YAHOO <- stockDataDownload(stock_symbols = SP500_symbols, from = "2008-12-01", to = "2018-12-01")
save(SP500_YAHOO, file = "data-raw/SP500_YAHOO.RData", version = 2)

sapply(SP500_YAHOO, dim)  # sanity check

dataset <- stockDataResample(SP500_YAHOO, N_stock = 50, T_sample = 252*2, N_dataset = 10)
save(dataset, file = "data-raw/dataset.RData")

checkDataset <- function(dataset) {
  for (i in 1:length(dataset)) {
    single_dataset <- dataset[[i]]
    cat("-------------------Checking", i, "-th element--------------------\n")
    cat("Any NA exists?", anyNA(single_dataset), "\n")
    print(sapply(single_dataset, dim))
    
    open_name <- sub(".Open", "",colnames(single_dataset$open))
    high_name <- sub(".High", "", colnames(single_dataset$high))
    low_name <- sub(".Low", "",colnames(single_dataset$low))
    close_name <- sub(".Close", "", colnames(single_dataset$close))
    volume_name <- sub(".Volume", "",colnames(single_dataset$volume))
    adj_name <- sub(".Adjusted", "", colnames(single_dataset$adjusted))
    
    name_check <- apply(cbind(open_name, high_name, low_name, close_name, volume_name, adj_name), 
                        2, function(x){all(x == open_name)})
    print(name_check)
  }
}
