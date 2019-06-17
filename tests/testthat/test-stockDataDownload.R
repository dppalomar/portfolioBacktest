context("Checking online data downloading")

# stockDataDownload_check <- stockDataDownload(c("AAPL", "MSFT"), from = "2018-01-01", to = "2018-01-30")
# stockDataDownload_check <- lapply(stockDataDownload_check, coredata)
# save(stockDataDownload_check, file = "stockDataDownload_check.RData")

load("stockDataDownload_check.RData")

test_that("the embedded SP500 symbols are the same", {
  sink(file = tempfile())
  stockData <- stockDataDownload(c("AAPL", "MSFT"), from = "2018-01-01", to = "2018-01-30")
  sink()
  stockData <- lapply(stockData, coredata)
  expect_equal(stockData, stockDataDownload_check)
})
