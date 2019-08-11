## Changes in portfolioBacktest version 0.1.2 (2019-XX-XX)

* Now stockDataDownload() will store the downloaded data into a local file 
  and if called again with the same arguments will use it (Issue: #2).
  
* Function portfolioBacktest() now returns two portfolios: w_designed and w_bop.

* Function portfolioBacktest() now takes an extra argument for the portfolio execution
  which can be "same day" or "next day".
  
* Transaction costs are now included in the backtest computation and function 
  portfolioBacktest() takes an extra argument.


## Changes in portfolioBacktest version 0.1.1 (2019-07-06)

* Problems with test (regarding stockDataDownload) fixed.
* Problem with table in README fixed.


## Changes in portfolioBacktest version 0.1.0 (2019-06-19)

* Initial release is on CRAN.