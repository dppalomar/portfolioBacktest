library(quantmod)

IBM <- getSymbols("IBM", from = "2018-01-01", to = "2019-01-01", auto.assign = FALSE)
saveSymbols("IBM", file.path = ".")

# you can load now from the file, but cannot specify from and to dates
IBM <- getSymbols("IBM", from = "2018-01-01", to = "2018-02-01", src = "RData", extension = "RData", auto.assign = FALSE)

# Conclusion: the best way would be to define a new function called getSymbols.XXX and then call getSymbol() specifying src = "XXX"