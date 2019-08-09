library(portfolioBacktest)
library(xts)
data("dataset10")

# stock data
prices <- dataset10[[1]]$adjusted[1:100, 1:2]
colnames(prices) <- c("stock1", "stock2")
X_lin <- (prices/lag(prices) - 1)[-1]
N <- ncol(X_lin)  # number of stocks
T <- nrow(X_lin)  # number of days

#
# daily rebalancing of EWP
#
# backtest EWP directly
w_EWP <- rep(1/N, N)
ret_direct <- xts(X_lin %*% w_EWP, index(X_lin))
wealth_direct <- xts(c(1, cumprod(1 + ret_direct)), index(prices))  # compounded (initial budget of 1$)

# backtest EWP with PerformanceAnalytics
library(PerformanceAnalytics)
PerfAnal <- Return.portfolio(X_lin, weights = w_EWP, rebalance_on = "days", verbose = TRUE)
all.equal(ret_direct, PerfAnal$returns, check.attributes = FALSE)

# backtest EWP with portfolioBacktest
uniform_portfolio <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), 
                        dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                        T_rolling_window = 1,
                        optimize_every = 1, 
                        rebalance_every = 1,
                        return_portfolio = TRUE, 
                        return_returns = TRUE)
ret_portfolioBacktest <- bt$Uniform$`dataset 1`$return
wealth_portfolioBacktest <- bt$Uniform$`dataset 1`$cumPnL
all.equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
all.equal(bt$Uniform$`dataset 1`$w_bop,
          PerfAnal$BOP.Weight, check.attributes = FALSE)




#
# EWP rebalanced every 20 days
# 
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio), 
                        dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                        T_rolling_window = 20,
                        optimize_every = 20, 
                        rebalance_every = 20,
                        return_portfolio = TRUE, 
                        return_returns = TRUE)
ret_portfolioBacktest <- bt$Uniform$`dataset 1`$return
wealth_portfolioBacktest <- bt$Uniform$`dataset 1`$cumPnL
head(bt$Uniform$`dataset 1`$w_designed)
head(bt$Uniform$`dataset 1`$w_bop)

# sanity check:
PerfAnal <- Return.portfolio(X_lin, weights = bt$Uniform$`dataset 1`$w_designed, verbose = TRUE)
all.equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
all.equal(bt$Uniform$`dataset 1`$w_bop,
          PerfAnal$BOP.Weight, check.attributes = FALSE)






#
# GMVP rebalanced every 20 days
# 
GMVP_portfolio <- function(data) {
  X <- diff(log(data$adjusted))[-1]
  Sigma <- cov(X)
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))
  return(w)
}
bt <- portfolioBacktest(list("Uniform" = GMVP_portfolio),
                        dataset_list = list("dataset 1" = list("adjusted" = prices)),  # just one single dataset!
                        T_rolling_window = 20,
                        optimize_every = 20, 
                        rebalance_every = 20,
                        return_portfolio = TRUE, 
                        return_returns = TRUE)
ret_portfolioBacktest <- bt$Uniform$`dataset 1`$return
wealth_portfolioBacktest <- bt$Uniform$`dataset 1`$cumPnL
head(bt$Uniform$`dataset 1`$w_designed)
head(bt$Uniform$`dataset 1`$w_bop)

# sanity check:
PerfAnal <- Return.portfolio(X_lin, weights = bt$Uniform$`dataset 1`$w_designed, verbose = TRUE)
all.equal(ret_portfolioBacktest, PerfAnal$returns, check.attributes = FALSE)
all.equal(bt$Uniform$`dataset 1`$w_bop,
          PerfAnal$BOP.Weight[, 1:2], check.attributes = FALSE)
cash <- 1 - rowSums(bt$Uniform$`dataset 1`$w_bop)
all.equal(cash, as.vector(PerfAnal$BOP.Weight[, 3]), check.attributes = FALSE)











# # backtest EWP with PMwR::btest()
# library(PMwR)
# 
# ew <- function () {
#   N <- ncol(Close())
#   rep(1/N, N)
# }
# 
# bt.ew <- btest(prices = list(coredata(prices)),
#                signal = ew,
#                do.signal = 1:nrow(prices),
#                convert.weights = TRUE,
#                initial.cash = 1,
#                b = 1,
#                timestamp = index(prices),
#                instrument = colnames(prices))
# str(bt.ew)
# xts(bt.ew$suggested.position, bt.ew$timestamp)
# xts(bt.ew$position, bt.ew$timestamp)
# 
# bt.ew$wealth
# wealth_direct

