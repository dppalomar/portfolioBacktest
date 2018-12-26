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
Backtesting of a portfolio in a rolling-window fashion over a dataset of stock prices. Multiple datasets are allowed (e.g., taken randomly over different markets, different time periods, and different subset of the stock universe). In addition, multiple portfolios can be backtested for a subsequent comparison and ranking on a number of criteria including annual return, annual volatility, Sharpe ratio, maximum drawdown, turnover rate, return on investment, computational time, etc. The portfolio is defined as a function that takes as input a window of the stock prices and outputs the portfolio weights. This package can be useful for a researcher/practitioner who wants to backtest a set of portfolios over a multitude of datasets over different markets. In addition, it can be particularly useful to evaluate students in a portfolio design course where the grading is based on the performance.


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
library(PerformanceAnalytics)
library(portfolioBacktest)
data(dataset) 
```
The dataset `dataset` is a list of data that contains the prices of random sets of stock market data from the S&P 500, over random periods of two years with a random selection of 50 stocks of each universe.
 

```r
length(dataset)
#> [1] 10
str(dataset[[1]])
#> List of 2
#>  $ prices:An 'xts' object on 2013-07-03/2015-07-02 containing:
#>   Data: num [1:504, 1:50] 51.2 51.8 51.8 51.9 52.3 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:50] "MSI" "CCI" "CMG" "KSU" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#> List of 2
#>   ..$ src    : chr "yahoo"
#>   ..$ updated: POSIXct[1:1], format: "2018-12-05 14:26:04"
#>  $ index :An 'xts' object on 2013-07-03/2015-07-02 containing:
#>   Data: num [1:504, 1] 1615 1632 1640 1652 1653 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "INDEX"
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#> List of 2
#>   ..$ src    : chr "yahoo"
#>   ..$ updated: POSIXct[1:1], format: "2018-12-05 18:59:49"

colnames(dataset[[1]]$prices)
#>  [1] "MSI"  "CCI"  "CMG"  "KSU"  "CSCO" "DLTR" "GLW"  "FLIR" "AVGO" "JWN" 
#> [11] "XLNX" "STZ"  "XEL"  "VZ"   "SYY"  "IFF"  "MU"   "CSX"  "DFS"  "ILMN"
#> [21] "XOM"  "HP"   "WM"   "WEC"  "CNP"  "HBI"  "INCY" "IT"   "HBAN" "ISRG"
#> [31] "TIF"  "AMAT" "GD"   "NFLX" "ETR"  "CI"   "MSCI" "COTY" "FE"   "ICE" 
#> [41] "DIS"  "TEL"  "PM"   "ALB"  "MCD"  "ALL"  "EQR"  "MAS"  "PEP"  "KEY"
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

We are then ready to use the function `portfolioBacktest()` that will execute and evaluate the portfolio design function on a rolling-window basis, and the result can be easily handled with privided function `backtestSelector()`

```r
bt <- portfolioBacktest(portfolio_fun, dataset[1], shortselling = TRUE)
res <- backtestSelector(bt)
names(res)
#> [1] "performance"   "error"         "error_message" "cpu_time"     
#> [5] "portfolio"     "return"        "cumPnL"
plot(res$cumPnL[[1]])
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

```r
res$performance
#>      Sharpe ratio max drawdown annual return annual volatility
#> [1,]    0.4437945   0.03670731    0.02007334        0.04523115
#>      Sterling ratio Omega ratio  ROT bps
#> [1,]      0.5468486    1.078093 48.09867
```

We can also backtest over multiple data sets 

```r
# perform multiple backtesting
mul_data_bt <- portfolioBacktest(portfolio_fun, dataset,shortselling = TRUE)
mul_data_res <- backtestSelector(mul_data_bt)
mul_data_res$performance
#>       Sharpe ratio max drawdown annual return annual volatility
#>  [1,]    0.4437945 3.670731e-02   0.020073339      4.523115e-02
#>  [2,]    2.3859666 1.510183e-02   0.076737026      3.216182e-02
#>  [3,]    1.2541919 4.576403e-02   0.045353149      3.616125e-02
#>  [4,]    1.7950207 5.997073e-06   0.000019294      1.074862e-05
#>  [5,]    0.7189881 2.528428e-02   0.030081997      4.183935e-02
#>  [6,]    1.5927899 2.966404e-02   0.063387853      3.979674e-02
#>  [7,]    0.7054128 3.222290e-02   0.028171124      3.993566e-02
#>  [8,]    2.0424434 1.899085e-02   0.077353615      3.787308e-02
#>  [9,]    1.2835515 3.689461e-02   0.050298393      3.918689e-02
#> [10,]    1.4705575 2.752320e-02   0.045738098      3.110256e-02
#>       Sterling ratio Omega ratio     ROT bps
#>  [1,]      0.5468486    1.078093  48.0986690
#>  [2,]      5.0813056    1.442469 191.4189698
#>  [3,]      0.9910218    1.227148 114.9065118
#>  [4,]      3.2172361    1.365454   0.2507008
#>  [5,]      1.1897511    1.129900  74.0246684
#>  [6,]      2.1368585    1.302004 125.1864196
#>  [7,]      0.8742580    1.122554  87.2840141
#>  [8,]      4.0732047    1.378620 193.1474015
#>  [9,]      1.3632994    1.230155 103.8112961
#> [10,]      1.6618016    1.258987 136.3864955
```

For comparison, we may want some benchmarks. Now the package suppport two benchmarks, which are `uniform portfolio` and `index` of the certain market. We can easily do that 


```r
mul_data_bt <- portfolioBacktest(portfolio_fun, dataset, benchmark = c("uniform", "index"), shortselling = TRUE)
names(mul_data_bt)
#> [1] "fun1"    "uniform" "index"
```

Then we can extract the desired result by using passing the corresponding name to argument `portfolio_name` of function `backtestSelector()`


```r
# extract result of the passed function
res_fun1 <- backtestSelector(mul_data_bt, "fun1")
names(res_fun1)
#> [1] "performance"   "error"         "error_message" "cpu_time"     
#> [5] "portfolio"     "return"        "cumPnL"
res_fun1$performance
#>       Sharpe ratio max drawdown annual return annual volatility
#>  [1,]    0.4437945 3.670731e-02   0.020073339      4.523115e-02
#>  [2,]    2.3859666 1.510183e-02   0.076737026      3.216182e-02
#>  [3,]    1.2541919 4.576403e-02   0.045353149      3.616125e-02
#>  [4,]    1.7950207 5.997073e-06   0.000019294      1.074862e-05
#>  [5,]    0.7189881 2.528428e-02   0.030081997      4.183935e-02
#>  [6,]    1.5927899 2.966404e-02   0.063387853      3.979674e-02
#>  [7,]    0.7054128 3.222290e-02   0.028171124      3.993566e-02
#>  [8,]    2.0424434 1.899085e-02   0.077353615      3.787308e-02
#>  [9,]    1.2835515 3.689461e-02   0.050298393      3.918689e-02
#> [10,]    1.4705575 2.752320e-02   0.045738098      3.110256e-02
#>       Sterling ratio Omega ratio     ROT bps
#>  [1,]      0.5468486    1.078093  48.0986690
#>  [2,]      5.0813056    1.442469 191.4189698
#>  [3,]      0.9910218    1.227148 114.9065118
#>  [4,]      3.2172361    1.365454   0.2507008
#>  [5,]      1.1897511    1.129900  74.0246684
#>  [6,]      2.1368585    1.302004 125.1864196
#>  [7,]      0.8742580    1.122554  87.2840141
#>  [8,]      4.0732047    1.378620 193.1474015
#>  [9,]      1.3632994    1.230155 103.8112961
#> [10,]      1.6618016    1.258987 136.3864955

# extract result of the uniform portfolio function
res_uniform <- backtestSelector(mul_data_bt, "uniform")
names(res_uniform)
#> [1] "performance"   "error"         "error_message" "cpu_time"     
#> [5] "portfolio"     "return"        "cumPnL"
```

For a clear view, we can summarize all the portfolios' performance based on user customized summary functions. For example, we want to compare the median and average value of the performance of these portfolios.


```r
res_summary <- backtestSummary(mul_data_bt, summary_funs = list('median' = median, 'mean' = mean))
names(res_summary)
#> [1] "performance_summary_median" "performance_summary_mean"  
#> [3] "cpu_time_average"           "failure_rate"              
#> [5] "error_message"
res_summary$performance_summary_median
#>         Sharpe ratio max drawdown annual return annual volatility
#> fun1        1.377054   0.02859362    0.04554562        0.03852998
#> uniform     1.576826   0.07635119    0.19351495        0.13079670
#> index       1.070965   0.07506676    0.13313531        0.12256102
#>         Sterling ratio Omega ratio   ROT bps
#> fun1          1.512551    1.244571  109.3589
#> uniform       2.462373    1.285195 3662.3918
#> index         1.510728    1.207702       Inf
res_summary$performance_summary_mean
#>         Sharpe ratio max drawdown annual return annual volatility
#> fun1        1.369272   0.02681590    0.04372139        0.03432993
#> uniform     1.501213   0.08562760    0.20117883        0.13485154
#> index       1.102840   0.08298395    0.12858558        0.12074051
#>         Sterling ratio Omega ratio   ROT bps
#> fun1          2.113559    1.253538  107.4515
#> uniform       2.782111    1.274863 3458.5735
#> index         1.774171    1.212180       Inf
```



## Links
Package: [GitHub](https://github.com/dppalomar/portfolioBacktest).  
README file: [GitHub-readme](https://rawgit.com/dppalomar/portfolioBacktest/master/README.html).  
Vignette: [GitHub-html-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.html) and [GitHub-pdf-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.pdf).

