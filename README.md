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
?prices
```

Usage of `backtestPortfolio()`
------------------------------

We start by loading the package and some market data from the underlying assets of the S&P 500:

``` r
library(backtestPortfolio)
library(xts)
data(prices)
```

The list `prices` contains random selections of *N* = 50 stocks for periods of two years from the S&P 500.

``` r
length(prices)
#> [1] 30
str(prices[[1]])
#> An 'xts' object on 1999-05-05/2001-05-02 containing:
#>   Data: num [1:504, 1:50] 0.0769 -0.0653 0.0146 0.0305 -0.0218 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:50] "MU UN Equity" "C UN Equity" "CTB UN Equity" "ROH UN Equity" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#>  NULL

colnames(prices[[1]])
#>  [1] "MU UN Equity"       "C UN Equity"        "CTB UN Equity"     
#>  [4] "ROH UN Equity"      "DOW UN Equity"      "3026360Q UN Equity"
#>  [7] "TWX UN Equity"      "EXC UN Equity"      "ADM UN Equity"     
#> [10] "CI UN Equity"       "PH UN Equity"       "AAPL UQ Equity"    
#> [13] "RX UN Equity"       "3403545Q UN Equity" "XEL UN Equity"     
#> [16] "LNC UN Equity"      "ROK UN Equity"      "GLW UN Equity"     
#> [19] "AEP UN Equity"      "MCO UN Equity"      "SHW UN Equity"     
#> [22] "BF/B UN Equity"     "COP UN Equity"      "WHR UN Equity"     
#> [25] "KR UN Equity"       "CIN UN Equity"      "BMS UN Equity"     
#> [28] "HRB UN Equity"      "0544749D UN Equity" "PTC UQ Equity"     
#> [31] "HON UN Equity"      "PLL UN Equity"      "SO UN Equity"      
#> [34] "HIG UN Equity"      "1288652D US Equity" "ETR UN Equity"     
#> [37] "EMC UN Equity"      "COF UN Equity"      "MAS UN Equity"     
#> [40] "LU UN Equity"       "LOW UN Equity"      "WFC UN Equity"     
#> [43] "DJ UN Equity"       "FCX UN Equity"      "NOVL UQ Equity"    
#> [46] "BOL UN Equity"      "K UN Equity"        "RSHCQ UN Equity"   
#> [49] "MAR UN Equity"      "PEP UN Equity"
```

Now, we define some portfolio design that takes as input the prices and outputs the portfolio vector `w`:

``` r
# load all the required libraries for the portfolio design
library(CVXR)
library(xts)

naivePortfolioDesign <- function(prices) {
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

We are then ready to use the function `backtestPortfolio()` that will execute the portfolio design function on a rolling-window basis:

``` r
res <- backtestPortfolio(naivePortfolioDesign, prices[[1]])
print(res)
```

Links
-----

Package: [GitHub](https://github.com/dppalomar/backtestPortfolio).
README file: [GitHub-readme](https://rawgit.com/dppalomar/backtestPortfolio/master/README.html).
