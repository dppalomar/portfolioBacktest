## Changes in portfolioBacktest version 0.2.1 (2019-10-07)

* Bug fixed with global variables when using paral_portfolios > 1.

* Typos fixed in vignette.

* Data SP500_symbols updated.

* Bug fix in backtestLeaderboard() when some portfolios have 100% failure rate.

* Three new plotting function: backtestChartCumReturns(), backtestChartDrawdown(), backtestChartStackedBar().


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