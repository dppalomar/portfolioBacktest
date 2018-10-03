library(xts)
library(portfolioBacktest)
data(prices)

kk <- 2
for (i in 1:ncol(prices[[kk]])) {
  print(plot(prices[[kk]][, i]))
  readline(prompt="Press [enter] to continue")
}
