library(xts)

test <- xts(rnorm(3), order.by = as.POSIXct(c("2014-01-01 19:00:00 HKT", "2014-01-02 19:00:00 HKT", "2014-01-05 19:00:00 HKT")))
tclass(test)  # indexClass(test)
test["2014-01-01"]
index(test["2014-01-01"])
test[index(test["2014-01-01"])]

test2 <- convertIndex(test, "Date")
tclass(test2)  # indexClass(test2)
test2["2014-01-01"]
index(test2["2014-01-01"])
test2[index(test2["2014-01-01"])]  # What???!!!  Empty!!!

test3 <- xts(rnorm(3), order.by = as.Date(c("2014-01-01 19:00:00 HKT", "2014-01-02 19:00:00 HKT", "2014-01-05 19:00:00 HKT")))
indexClass(test3)
test3["2014-01-01"]
index(test3["2014-01-01"])
test3[index(test3["2014-01-01"])]






#########################################
library(portfolioBacktest)

faang_log_returns <- as.xts(read.zoo('https://raw.githubusercontent.com/souzatharsis/open-quant-live-book/master/data/FAANG.csv',
                            header = TRUE, index.column=1, sep=","))
faang_log_returns <- faang_log_returns["2014-01-01/2019-09-01"]
head(faang_log_returns)
data_index_converted <- faang_log_returns
tclass(data_index_converted) <- "Date"
#data_index_converted <- convertIndex(faang_log_returns,'Date')
head(data_index_converted)
class(index(data_index_converted))


# This is the normal behaviour of indexing xts:
lala <- dataset10[[1]]$adjusted
indexClass(lala)
tclass(lala)
lala["2015-04-24"]
lala[as.character(index(lala["2015-04-24"])), ]
lala[index(lala["2015-04-24"]), ]


# Something wrong happens here. It seems the conversion to type Date didn't work somehow:
tclass(data_index_converted)
data_index_converted["2014-12-30"]
data_index_converted[as.character(index(data_index_converted["2014-12-30"])), ]
data_index_converted[xts:::index.xts(data_index_converted["2014-12-30"]), ]


# I tried to fix additional attributes but still no success
attr(lala, ".indexTZ")
attr(data_index_converted, ".indexTZ") <- "UTC"

attr(lala, "tclass")
attr(data_index_converted, "tclass") <- "Date"

attr(lala, "tzone")
attr(data_index_converted, "tzone") <- "UTC"


.index(lala)
.indexday(lala)
.indexday(data_index_converted)





# No need to even try the backtesting:
prices <- cumprod(data_index_converted + 1)
dataset <- list(dataset1 = list(adjusted = prices))

uniform_portfolio_fun <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}

bt <- portfolioBacktest(portfolio_funs = list(uniform_portfolio_fun), dataset_list = dataset, show_progress_bar = TRUE)
