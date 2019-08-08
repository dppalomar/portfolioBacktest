context("Checking online data downloading")

test_that("the embedded SP500 symbols are the same", {
  sink(file = tempfile())
  stockData <- stockDataDownload(c("AAPL"), from = "2018-01-01", to = "2018-01-30", local_file_path = NULL)
  sink()
  stockData <- as.numeric(lapply(stockData, mean))
  stockDataDownload_check <- quantmod::getSymbols("AAPL", from = "2018-01-01", to = "2018-01-30", auto.assign = FALSE)
  expect_equal(as.numeric(lapply(stockData, mean)), as.numeric(colMeans(stockDataDownload_check)))
})
