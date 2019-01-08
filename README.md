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
names(dataset[[1]])
#> [1] "open"     "high"     "low"      "close"    "volume"   "adjusted"
#> [7] "index"
str(dataset[[1]]$adjusted)
#> An 'xts' object on 2015-04-24/2017-04-24 containing:
#>   Data: num [1:504, 1:50] 22.1 22.1 22.7 22.5 22.3 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:50] "MAS.Adjusted" "MGM.Adjusted" "CMI.Adjusted" "CSX.Adjusted" ...
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#> List of 2
#>  $ src    : chr "yahoo"
#>  $ updated: POSIXct[1:1], format: "2018-12-29 16:24:41"

colnames(dataset[[1]]$adjusted)
#>  [1] "MAS.Adjusted"  "MGM.Adjusted"  "CMI.Adjusted"  "CSX.Adjusted" 
#>  [5] "TGT.Adjusted"  "AWK.Adjusted"  "LNC.Adjusted"  "KO.Adjusted"  
#>  [9] "CCI.Adjusted"  "RJF.Adjusted"  "ICE.Adjusted"  "SRE.Adjusted" 
#> [13] "FOXA.Adjusted" "CERN.Adjusted" "ORLY.Adjusted" "EMR.Adjusted" 
#> [17] "CME.Adjusted"  "AVB.Adjusted"  "AMT.Adjusted"  "TIF.Adjusted" 
#> [21] "HAL.Adjusted"  "OMC.Adjusted"  "NTAP.Adjusted" "KORS.Adjusted"
#> [25] "AEP.Adjusted"  "A.Adjusted"    "KSS.Adjusted"  "BHGE.Adjusted"
#> [29] "BEN.Adjusted"  "HST.Adjusted"  "AMP.Adjusted"  "WY.Adjusted"  
#> [33] "AGN.Adjusted"  "CPB.Adjusted"  "NWL.Adjusted"  "INTC.Adjusted"
#> [37] "XRAY.Adjusted" "VRSK.Adjusted" "MLM.Adjusted"  "CI.Adjusted"  
#> [41] "PHM.Adjusted"  "MKC.Adjusted"  "OXY.Adjusted"  "GM.Adjusted"  
#> [45] "CB.Adjusted"   "RHT.Adjusted"  "DOV.Adjusted"  "GLW.Adjusted" 
#> [49] "FLIR.Adjusted" "GPC.Adjusted"
```

Now, we define some portfolio design that takes as input the prices and outputs the portfolio vector `w`:

```r
portfolio_fun <- function(data) {
  X <- diff(log(data$adjusted))[-1]  # compute log returns, here we use adjusted prices
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
res <- backtestSelector(bt, portfolio_index = 1)
names(res)
#> [1] "performance"   "error"         "error_message" "cpu_time"     
#> [5] "portfolio"     "return"        "cumPnL"
plot(res$cumPnL[[1]])
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

We can also backtest over multiple data sets 

```r
# perform multiple backtesting
mul_data_bt <- portfolioBacktest(portfolio_fun, dataset, shortselling = TRUE)
mul_data_res <- backtestSelector(mul_data_bt, portfolio_index = 1)
mul_data_res$performance
#>       Sharpe ratio max drawdown annual return annual volatility
#>  [1,]   0.43216171   0.02601129   0.018310402        0.04236933
#>  [2,]   0.45708668   0.04323931   0.024808463        0.05427518
#>  [3,]   1.41115502   0.02286482   0.046456693        0.03292104
#>  [4,]  -0.01951969   0.06783961  -0.001085763        0.05562397
#>  [5,]   0.72256088   0.06083075   0.041099042        0.05687970
#>  [6,]   2.17578547   0.02212631   0.071331145        0.03278409
#>  [7,]   4.32024112   0.01591036   0.139474565        0.03228398
#>  [8,]   1.13401883   0.04077619   0.051204767        0.04515337
#>  [9,]   0.86003940   0.07066204   0.049092802        0.05708204
#> [10,]   1.82664160   0.03751242   0.057249236        0.03134125
#>       Sterling ratio Omega ratio     ROT bps
#>  [1,]     0.70394064    1.075537  36.4135924
#>  [2,]     0.57374788    1.083725  60.4375741
#>  [3,]     2.03179820    1.251251 122.4139059
#>  [4,]    -0.01600485    1.001542  -0.7599049
#>  [5,]     0.67562935    1.142782 108.8563347
#>  [6,]     3.22381606    1.393876 159.0092504
#>  [7,]     8.76627360    1.910817 352.9881774
#>  [8,]     1.25575149    1.222673 129.9534162
#>  [9,]     0.69475494    1.164732  86.8431384
#> [10,]     1.52614101    1.343066 142.6190387
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
#>  [1,]   0.43216171   0.02601129   0.018310402        0.04236933
#>  [2,]   0.45708668   0.04323931   0.024808463        0.05427518
#>  [3,]   1.41115502   0.02286482   0.046456693        0.03292104
#>  [4,]  -0.01951969   0.06783961  -0.001085763        0.05562397
#>  [5,]   0.72256088   0.06083075   0.041099042        0.05687970
#>  [6,]   2.17578547   0.02212631   0.071331145        0.03278409
#>  [7,]   4.32024112   0.01591036   0.139474565        0.03228398
#>  [8,]   1.13401883   0.04077619   0.051204767        0.04515337
#>  [9,]   0.86003940   0.07066204   0.049092802        0.05708204
#> [10,]   1.82664160   0.03751242   0.057249236        0.03134125
#>       Sterling ratio Omega ratio     ROT bps
#>  [1,]     0.70394064    1.075537  36.4135924
#>  [2,]     0.57374788    1.083725  60.4375741
#>  [3,]     2.03179820    1.251251 122.4139059
#>  [4,]    -0.01600485    1.001542  -0.7599049
#>  [5,]     0.67562935    1.142782 108.8563347
#>  [6,]     3.22381606    1.393876 159.0092504
#>  [7,]     8.76627360    1.910817 352.9881774
#>  [8,]     1.25575149    1.222673 129.9534162
#>  [9,]     0.69475494    1.164732  86.8431384
#> [10,]     1.52614101    1.343066 142.6190387

# extract result of the uniform portfolio function
res_uniform <- backtestSelector(mul_data_bt, "uniform")
names(res_uniform)
#> [1] "performance"   "error"         "error_message" "cpu_time"     
#> [5] "portfolio"     "return"        "cumPnL"
```

For a clear view, we can summarize all the portfolios' performance based on user customized summary functions. For example, we want to compare the median and average value of the performance of these portfolios.


```r
res_summary <- backtestSummary(mul_data_bt, summary_fun = median)
names(res_summary)
#> [1] "performance_summary" "failure_rate"        "cpu_time_average"   
#> [4] "error_message"
res_summary$performance_summary
#>                           fun1      uniform      index
#> Sharpe ratio        0.99702912 1.546805e+00 1.33208392
#> max drawdown        0.03914431 8.946477e-02 0.09169451
#> annual return       0.04777475 1.651707e-01 0.14897463
#> annual volatility   0.04376135 1.215642e-01 0.12479190
#> Sterling ratio      0.97984607 2.219609e+00 1.93644619
#> Omega ratio         1.19370264 1.296275e+00 1.27258183
#> ROT bps           115.63512029 2.715873e+03        Inf
```



## Links
Package: [GitHub](https://github.com/dppalomar/portfolioBacktest).  
README file: [GitHub-readme](https://rawgit.com/dppalomar/portfolioBacktest/master/README.html).  
Vignette: [GitHub-html-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.html) and [GitHub-pdf-vignette](https://rawgit.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest-vignette.pdf).

