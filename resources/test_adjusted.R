library(quantmod)

IBM <- getSymbols("IBM", from = "2018-01-01", to = "2019-01-01", auto.assign = FALSE)
IBM <- getSymbols("IBM", from = "2018-01-01", auto.assign = FALSE)
head(IBM)
plot(cbind(Cl(IBM), Ad(IBM)))
plot(Cl(IBM)/Ad(IBM))


IBM_adj1 <- adjustOHLC(IBM, use.Adjusted = TRUE)
head(IBM_adj1)

IBM_adj2 <- adjustOHLC(IBM)
head(IBM_adj2)


tail(IBM)
tail(IBM_adj1)
tail(IBM_adj2)

#
# Notes:
#   1) With "use.Adjusted = TRUE", the adjustment is based on the XXX.Adjusted column which is computed by Yahoo as of today.
#   2) With "use.Adjusted = FALSE", the adjustment is based on dividends as of the last day of the xts.
#   3) If the last day of the xts is today, then both methods are similar. Otherwise, they will be quite different.
#   4) For backtesting, the most realistic way would be to use "use.Adjusted = FALSE" so that the adjustment is done based on 
#      the last day of the windowed data used for backtesting.
#