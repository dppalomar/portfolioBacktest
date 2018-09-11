<!-- README.md is generated from README.Rmd. Please edit that file -->
backtestPortfolio
=================

Backtest of a portfolio design with real data taken randomly from different markets and selecting randomly a subset of stocks for proper assessments. It also allows to backtest multiple portfolio designs and rank them based on a number of criteria including expected return, Sharpe ratio, drawdown, etc.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("dppalomar/backtestPortfolio")

# Getting help
library(backtestPortfolio)
help(package = "backtestPortfolio")
package?backtestPortfolio
?backtestPortfolio
?prices1
```

Usage of `backtestPortfolio()`
------------------------------

We start by loading the package and some market data from the underlying assets of the S&P 500:

``` r
library(backtestPortfolio)
library(xts)
data(prices1)
```

The data `prices1` contains the log-prices of a randomly selected sample of *N* = 50 stocks from the S&P 500 (the package also contains four other sets or randomly selected data, namely, `prices2`, `prices3`, `prices4`, and `prices5`).

``` r
str(prices1)
#> An 'xts' object on 2010-01-04/2010-12-31 containing:
#>   Data: num [1:252, 1:50] 0.01008 0.00684 -0.02322 -0.00667 0.01109 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:50] "EXPD UW Equity" "CELG UW Equity" "LLY UN Equity" "PPL UN Equity" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#>  NULL
colnames(prices1)
#>  [1] "EXPD UW Equity" "CELG UW Equity" "LLY UN Equity"  "PPL UN Equity" 
#>  [5] "HOG UN Equity"  "CNX UN Equity"  "MDT UN Equity"  "DO UN Equity"  
#>  [9] "PCLN UW Equity" "BAC UN Equity"  "MYL UW Equity"  "MCO UN Equity" 
#> [13] "OXY UN Equity"  "DOW UN Equity"  "UNP UN Equity"  "EOG UN Equity" 
#> [17] "XRAY UW Equity" "TJX UN Equity"  "CSX UN Equity"  "PDCO UW Equity"
#> [21] "PNW UN Equity"  "FLS UN Equity"  "BMY UN Equity"  "LNC UN Equity" 
#> [25] "VTR UN Equity"  "FMC UN Equity"  "RHT UN Equity"  "XLNX UW Equity"
#> [29] "TRV UN Equity"  "MCK UN Equity"  "CTSH UW Equity" "WYNN UW Equity"
#> [33] "AIV UN Equity"  "CBG UN Equity"  "BRCM UW Equity" "CHRW UW Equity"
#> [37] "DNB UN Equity"  "EFX UN Equity"  "RHI UN Equity"  "TIF UN Equity" 
#> [41] "HIG UN Equity"  "CPB UN Equity"  "TMO UN Equity"  "CAT UN Equity" 
#> [45] "EQT UN Equity"  "LEN UN Equity"  "LH UN Equity"   "MJN UN Equity" 
#> [49] "AKAM UW Equity" "CBS UN Equity"
```

Now, we define some portfolio design that takes as input the prices and outputs the portfolio vector `w`:

``` r
naivePortfolioDesign <- function(prices) {
  # load all the required libraries
  library(CVXR)
  
  # compute log returns
  X <- diff(log(prices))[-1]
  
  # compute mean vector and SCM
  mu <- colMeans(X)
  Sigma <- cov(X)
  
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}
```

We are then ready to use the function `backtestPortfolio()` which will execute the portfolio design function on a rolling-based window:

``` r
res <- backtestPortfolio(naivePortfolioDesign, prices1)
print(res)
```

Links
-----

Package: [GitHub](https://github.com/dppalomar/backtestPortfolio).

README file: [GitHub-readme](https://rawgit.com/dppalomar/backtestPortfolio/master/README.html).
