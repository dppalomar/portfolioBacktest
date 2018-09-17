<!-- README.md is generated from README.Rmd. Please edit that file -->
portfolioBacktest
=================

Backtesting of a portfolio in a sliding-window fashion over a dataset of stock prices. Multiple datasets are allowed (e.g., taken randomly over different markets, different time periods, and different subset of the stock universe). In addition, multiple portfolios can be backtested for a subsequent comparison and ranking on a number of criteria including expected return, volatility, Sharpe ratio, maximum drawdown, turnover rate, return on investment, computational time, etc. The portfolio is defined as a function that takes as input a window of the stock prices and outputs the portfolio weights. This package can be useful for a researcher/practitioner who wants to backtest a set of portfolios over a multitude of datasets over different markets. In addition, it can be particularly useful to evaluate students in a portfolio design course where the grading is based on the performance.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("dppalomar/portfolioBacktest")

# Getting help
library(portfolioBacktest)
help(package = "portfolioBacktest")
package?portfolioBacktest
?portfolioBacktest
```

Usage of `portfolioBacktest()`
------------------------------

We start by loading the package and some random sets of stock market data:

``` r
library(xts)
library(portfolioBacktest)
data(prices)
```

The dataset `prices` is a list of objects `xts` that contains the prices of random sets of stock market data from the S&P 500, HSI, NKY, SHZ, and UKC, over random periods of two years with a random selection of 50 stocks of each universe.

``` r
length(prices)
#> [1] 40
str(prices[[1]])
#> An 'xts' object on 2014-02-21/2016-01-27 containing:
#>   Data: num [1:504, 1:47] 118 119 119 118 120 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:47] "1 HK Equity" "101 HK Equity" "1038 HK Equity" "1044 HK Equity" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#>  NULL

colnames(prices[[1]])
#>  [1] "1 HK Equity"    "101 HK Equity"  "1038 HK Equity" "1044 HK Equity"
#>  [5] "1088 HK Equity" "1093 HK Equity" "11 HK Equity"   "1109 HK Equity"
#>  [9] "1177 HK Equity" "12 HK Equity"   "1299 HK Equity" "1398 HK Equity"
#> [13] "151 HK Equity"  "16 HK Equity"   "17 HK Equity"   "175 HK Equity" 
#> [17] "19 HK Equity"   "1928 HK Equity" "2 HK Equity"    "2007 HK Equity"
#> [21] "2018 HK Equity" "2313 HK Equity" "2318 HK Equity" "2319 HK Equity"
#> [25] "2382 HK Equity" "2388 HK Equity" "2628 HK Equity" "267 HK Equity" 
#> [29] "27 HK Equity"   "3 HK Equity"    "3328 HK Equity" "386 HK Equity" 
#> [33] "388 HK Equity"  "3988 HK Equity" "5 HK Equity"    "6 HK Equity"   
#> [37] "66 HK Equity"   "688 HK Equity"  "700 HK Equity"  "762 HK Equity" 
#> [41] "823 HK Equity"  "83 HK Equity"   "836 HK Equity"  "857 HK Equity" 
#> [45] "883 HK Equity"  "939 HK Equity"  "941 HK Equity"
```

Now, we define some portfolio design that takes as input the prices and outputs the portfolio vector `w`:

``` r
portfolio_fun <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # normalized to have ||w||_1=1
  return(w)
}
```

We are then ready to use the function `backtestPortfolio()` that will execute and evaluate the portfolio design function on a rolling-window basis:

``` r
res <- portfolioBacktest(portfolio_fun, prices[[1]], shortselling = TRUE)
names(res)
#> [1] "returns"       "cumPnL"        "performance"   "time"         
#> [5] "error"         "error_message"
plot(res$cumPnL)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

``` r
res$performance
#>    sharpe ratio    max drawdown expected return      volatility 
#>     0.239021301     0.033779114     0.009339336     0.039073239
```

We can also backtest over multiple data sets

``` r
# perform multiple backtesting
mul_res <- portfolioBacktest(portfolio_fun, prices[1:5], shortselling = TRUE)
mul_res$performance
#>                        [,1]       [,2]       [,3]       [,4]       [,5]
#> sharpe ratio    0.239021301 1.48113180 0.96215352 1.19406713 1.74278525
#> max drawdown    0.033779114 0.02786928 0.03050584 0.03082488 0.01849429
#> expected return 0.009339336 0.05219351 0.03674541 0.04657384 0.06540040
#> volatility      0.039073239 0.03523894 0.03819079 0.03900437 0.03752637
mul_res$performance_summary
#>    sharpe ratio    max drawdown expected return      volatility 
#>      1.19406713      0.03050584      0.04657384      0.03819079
```

Links
-----

Package: [GitHub](https://github.com/dppalomar/portfolioBacktest).
README file: [GitHub-readme](https://rawgit.com/dppalomar/portfolioBacktest/master/README.html).
Vignette: [GitHub-html-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.html) and [GitHub-pdf-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.pdf).
