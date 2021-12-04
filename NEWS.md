
## Changes in portfolioBacktest version 0.3.1 (2021-10-11)

* Just a minor adjustment in the the unit tests to comply with CRAN.


## Changes in portfolioBacktest version 0.3.0 (2021-09-21)

* In the computation of Sharpe ratio and annualized return, uncompounded returns are used (before they were compounded).

* New function to add new performance measures: `add_performance()`

* Function name `backtestChartCumReturns()` changed to `backtestChartCumReturn()` .

* Now the portfolio function receives an additional argument `w_current` with the current portfolio. This allows for portfolio designs that take into account transaction costs and for smart rebalancing techniques.

* Lots of internal code rewritten to make it more robust to future coding bugs. Now use of ellipsis in most places (always with named arguments).


## Changes in portfolioBacktest version 0.2.3 (2021-01-12)

* Reimplement parallel mode using package `pbapply`.

* Add a temporary argument `source_to_local` in function `portfolioBacktest()` to address the issues of using package `CVXR` within files.

* Add MDP and MSRP as benchmarks.

* Fix performance computation when no investment happens in some days.

* Function `stockDataResample()` deprecated and revised as `financialDataResample()` to work with other than stock data (e.g., crypto data) and without requiring the elements `$adjusted` or `$index`.

* Package now works with non-daily data. For example, for hourly crypto data, one needs to specify `bars_per_year = 24*365`.


## Changes in portfolioBacktest version 0.2.2 (2020-07-29)

* Fix some CRAN small issue with function examples.

* New function to add new performance measure: `add_performance()`.

* Vignette revised (included references on the dangers of backtesting).


## Changes in portfolioBacktest version 0.2.1 (2019-10-07)

* Bug fixed with global variables when using paral_portfolios > 1.

* Typos fixed in vignette.

* Data SP500_symbols updated.

* Bug fix in backtestLeaderboard() when some portfolios have 100% failure rate.

* Three new plotting function: backtestChartCumReturns(), backtestChartDrawdown(), backtestChartStackedBar().

* Structural improvement for embedded benchmarks. Now it is easier to add more benchmarks.

* Add the global minimum variance portfolio (GMVP) with "shrinkage" option as a benchmark.

* Add two more performance criteria: VaR (alpha = 0.95) and CVaR (alpha = 0.95) of loss.

* Filter global variables by size: now the variables with size > 10 MB will not be transparent to parallel threads.

* Revised test examples.


## Changes in portfolioBacktest version 0.2.0 (2019-08-23)

* Now stockDataDownload() will store the downloaded data into a local file 
  and if called again with the same arguments will use it (Issue: #2).
  
* Function portfolioBacktest() now returns two portfolios: w_designed and w_bop.

* Function portfolioBacktest() now takes an extra argument for the portfolio execution
  which can be "same day" or "next day".
  
* Transaction costs are now included in the backtest computation and function 
  portfolioBacktest() takes an extra argument (Issue: #7).
  
* Two new functions for easy parameter tuning and plotting: genRandomFuns() and 
  plotPerformanceVsParams().
  
* Package ggplot2 is now imported and all the plots are based on it by default.


## Changes in portfolioBacktest version 0.1.1 (2019-07-06)

* Problems with test (regarding stockDataDownload) fixed.
* Problem with table in README fixed.


## Changes in portfolioBacktest version 0.1.0 (2019-06-19)

* Initial release is on CRAN.