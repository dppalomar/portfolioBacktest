---
output:
  html_document:
    variant: markdown_github
    keep_md: true
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# portfolioBacktest
Backtesting of a portfolio in a rolling-window fashion over a dataset of stock prices. Multiple datasets are allowed (e.g., taken randomly over different markets, different time periods, and different subset of the stock universe). In addition, multiple portfolios can be backtested for a subsequent comparison and ranking on a number of criteria including expected return, volatility, Sharpe ratio, maximum drawdown, turnover rate, return on investment, computational time, etc. The portfolio is defined as a function that takes as input a window of the stock prices and outputs the portfolio weights. This package can be useful for a researcher/practitioner who wants to backtest a set of portfolios over a multitude of datasets over different markets. In addition, it can be particularly useful to evaluate students in a portfolio design course where the grading is based on the performance.


## Installation

```r
# install.packages("devtools")
devtools::install_github("dppalomar/portfolioBacktest")

# Getting help
library(portfolioBacktest)
help(package = "portfolioBacktest")
package?portfolioBacktest
?portfolioBacktest
```


## Usage of `portfolioBacktest()`
We start by loading the package and some random sets of stock market data:

```r
library(xts)
library(portfolioBacktest)
data(dataset) 
```
The dataset `dataset` is a list of objects `xts` that contains the prices of random sets of stock market data from the S&P 500, over random periods of two years with a random selection of 50 stocks of each universe.
 

```r
length(dataset)
#> [1] 10
str(dataset[[1]])
#> List of 1
#>  $ prices:An 'xts' object on 2014-09-02/2016-08-30 containing:
#>   Data: num [1:504, 1:50] 18.8 19 19.3 19.2 19.1 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:50] "NVDA" "FL" "CDNS" "EIX" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#> List of 2
#>   ..$ src    : chr "yahoo"
#>   ..$ updated: POSIXct[1:1], format: "2018-12-05 13:30:48"

colnames(dataset[[1]]$prices)
#>  [1] "NVDA"  "FL"    "CDNS"  "EIX"   "HOLX"  "MCK"   "AZO"   "INCY" 
#>  [9] "IPG"   "ANSS"  "EW"    "INTC"  "HRB"   "BEN"   "LKQ"   "WFC"  
#> [17] "FRT"   "ICE"   "CB"    "COST"  "BLK"   "CMCSA" "NBL"   "SRCL" 
#> [25] "BMY"   "CAH"   "ED"    "D"     "CTAS"  "HP"    "ROP"   "CMA"  
#> [33] "TXN"   "ALGN"  "BAC"   "TRV"   "DVN"   "BIIB"  "DE"    "ABC"  
#> [41] "VTR"   "OKE"   "ADBE"  "GLW"   "NWSA"  "MAC"   "ADP"   "HD"   
#> [49] "HCA"   "AAPL"
```

Now, we define some portfolio design that takes as input the prices and outputs the portfolio vector `w`:

```r
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

```r
res <- portfolioBacktest(portfolio_fun, dataset[[1]], shortselling = TRUE)
names(res)
#> [1] "returns"       "cumPnL"        "performance"   "cpu_time"     
#> [5] "error"         "error_message"
plot(res$cumPnL)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

```r
res$performance
#>      Sharpe ratio      max drawdown     annual return annual volatility 
#>       -0.18563752        0.04528734       -0.00836890        0.04508195 
#>    Sterling ratio       Omega ratio           ROT bps 
#>       -0.18479559        0.97358349       -9.46725948
```

We can also backtest over multiple data sets 

```r
# perform multiple backtesting
mul_res <- portfolioBacktest(portfolio_fun, dataset[1:5], shortselling = TRUE)
mul_res$performance
#>                     dataset 1    dataset 2    dataset 3   dataset 4
#> Sharpe ratio      -0.18563752   2.52144320  0.151993558  0.75956378
#> max drawdown       0.04528734   0.03768676  0.039600713  0.03401692
#> annual return     -0.00836890   0.10100510  0.007531534  0.03756689
#> annual volatility  0.04508195   0.04005845  0.049551665  0.04945851
#> Sterling ratio    -0.18479559   2.68012171  0.190186826  1.10435903
#> Omega ratio        0.97358349   1.48967011  1.028987449  1.14186344
#> ROT bps           -9.46725948 222.89155669 22.239545666 92.59241894
#>                     dataset 5
#> Sharpe ratio       0.31696399
#> max drawdown       0.05959546
#> annual return      0.01779474
#> annual volatility  0.05614119
#> Sterling ratio     0.29859217
#> Omega ratio        1.06507217
#> ROT bps           41.42647550
mul_res$performance_summary
#>      Sharpe ratio (median)      max drawdown (median) 
#>                 0.31696399                 0.03960071 
#>     annual return (median) annual volatility (median) 
#>                 0.01779474                 0.04945851 
#>    Sterling ratio (median)       Omega ratio (median) 
#>                 0.29859217                 1.06507217 
#>           ROT bps (median) 
#>                41.42647550
```


## Links
Package: [GitHub](https://github.com/dppalomar/portfolioBacktest).  
README file: [GitHub-readme](https://rawgit.com/dppalomar/portfolioBacktest/master/README.html).  
Vignette: [GitHub-html-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.html) and [GitHub-pdf-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.pdf).

