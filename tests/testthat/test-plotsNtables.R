context("Checking whether additional packages are installed or not")

# first generate backtest result
bt <- portfolioBacktest(list("Uniform" = uniform_portfolio_fun), dataset10,
                        benchmark = c("uniform", "index"))
bt_summary_median <- backtestSummary(bt)



test_that("whether ggplot2 is installed for \"summaryBarPlot\"", {
  if (!requireNamespace("ggplot2", quietly = TRUE))  # if not installed
    expect_error(summaryBarPlot(bt_summary_median), "Please install package \"ggplot2\" or choose another plot type")
  else
    expect_silent(summaryBarPlot(bt_summary_median))
})


test_that("whether ggplot2 is installed for \"backtestBoxPlot\"", {
  if (!requireNamespace("ggplot2", quietly = TRUE))  # if not installed
    expect_error(backtestBoxPlot(bt), "Please install package \"ggplot2\" or choose another plot type")
  else
    expect_silent(backtestBoxPlot(bt))
})



test_that("whether DT is installed for \"summaryTable\"", {
  if (!requireNamespace("DT", quietly = TRUE))  # if not installed
    expect_error(summaryTable(bt_summary_median, type = "DT"), "Please install package \"DT\" or choose another table type")
  else
    expect_silent(summaryTable(bt_summary_median, type = "DT"))
})



test_that("whether gridExtra is installed for \"summaryTable\"", {
  if (!requireNamespace("gridExtra", quietly = TRUE))  # if not installed
    expect_error(summaryTable(bt_summary_median, type = "grid.table"), "Please install package \"gridExtra\" or choose another table type")
  else
    expect_silent(summaryTable(bt_summary_median, type = "grid.table"))
})