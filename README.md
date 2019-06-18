---
output:
  html_document:
    variant: markdown_github
    keep_md: true
  md_document:
    variant: markdown_github
always_allow_html: yes
---

<!-- README.md is generated from README.Rmd. Please edit that file -->





# portfolioBacktest
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/portfolioBacktest)](https://CRAN.R-project.org/package=portfolioBacktest)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/portfolioBacktest)](https://CRAN.R-project.org/package=portfolioBacktest)
[![CRAN Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/portfolioBacktest?color=brightgreen)](https://CRAN.R-project.org/package=portfolioBacktest)

Automated backtesting of multiple portfolios over multiple 
datasets of stock prices in a rolling-window fashion. Intended for 
researchers and practitioners to backtest a set of different portfolios, 
as well as by a course instructor to assess the students in their portfolio 
design in a fully automated and convenient manner, with results conveniently 
formatted in tables and plots. Each portfolio design is easily defined as a
function that takes as input a window of the stock prices and outputs the 
portfolio weights. Multiple portfolios can be easily specified as a list 
of functions or as files in a folder. Multiple datasets can be conveniently 
extracted randomly from different markets, different time periods, and 
different subsets of the stock universe. The results can be later assessed 
and ranked with tables based on a number of performance criteria (e.g., 
expected return, volatility, Sharpe ratio, drawdown, turnover rate, return 
on investment, computational time, etc.), as well as plotted in a number of 
ways with nice barplots and boxplots.


## Installation
The package can be installed from [CRAN](https://CRAN.R-project.org/package=portfolioBacktest) or [GitHub](https://github.com/dppalomar/portfolioBacktest):

```r
# install stable version from CRAN
install.packages("portfolioBacktest")

# install development version from GitHub
devtools::install_github("dppalomar/portfolioBacktest")
```

To get help:

```r
library(portfolioBacktest)
help(package = "portfolioBacktest")
?portfolioBacktest
```

To cite `portfolioBacktest` in publications:

```r
citation("portfolioBacktest")
```


## Quick Start
Do the backtest on your own portfolio following few steps:

- **Step 1** - load package & dataset

```r
library(portfolioBacktest)
#> Registered S3 method overwritten by 'xts':
#>   method     from
#>   as.zoo.xts zoo
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
data("dataset10")
```
- **Step 2** - define your own portfolio

```r
my_portfolio <- function(dataset) {
  prices <- dataset$adjusted
  N <- ncol(prices)
  return(rep(1/N, N))
}
```
- **Step 3** - do backtest

```r
bt <- portfolioBacktest(my_portfolio, dataset10)
#> Registered S3 method overwritten by 'R.oo':
#>   method        from       
#>   throw.default R.methodsS3
```
- **Step 4** - check your portfolio performance

```r
backtestSummary(bt)$performance
#>                          fun1
#> Sharpe ratio        1.5414027
#> max drawdown        0.0893789
#> annual return       0.1641528
#> annual volatility   0.1218623
#> Sterling ratio      2.2138192
#> Omega ratio         1.2950904
#> ROT bps           696.9894933
```

For a more detailed explanation on how to use the package with all the features, check the [vignette](https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html).



## Package Snapshot
This package backtests a list of portfolios over multiple datasets on a rolling-window basis, producing final results as in the following.



- Performance table:

<!--html_preserve--><div id="htmlwidget-448c5bf8b784d5d2659f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-448c5bf8b784d5d2659f">{"x":{"filter":"none","data":[["Quintile","GMVP","Markowitz","uniform","index"],[0.0029,0.0044,0.4529,0.0021,0],[1.1383,1.1686,0.573,1.61,1.0389],[0.1074,0.0313,0.226,0.0936,0.0907],[0.1939,0.0487,0.1495,0.1921,0.1323],[0.1613,0.0418,0.3165,0.1353,0.1249],[1.8075,1.4304,0.8002,2.1967,1.3762],[1.206,1.2152,1.1246,1.2923,1.2071],[253.2452,81.7284,187.1631,837.5048,null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>cpu time<\/th>\n      <th>Sharpe ratio<\/th>\n      <th>max drawdown<\/th>\n      <th>annual return<\/th>\n      <th>annual volatility<\/th>\n      <th>Sterling ratio<\/th>\n      <th>Omega ratio<\/th>\n      <th>ROT bps<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":15,"scrollX":true,"order":[2,"desc"],"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8]},{"orderable":false,"targets":0}],"autoWidth":false,"orderClasses":false,"lengthMenu":[10,15,25,50,100],"rowCallback":"function(row, data) {\nvar value=data[0]; $(row).css({'font-weight':value == 'uniform' ? 'bold' : value == 'index' ? 'bold' : ''});\nDTWidget.formatPercentage(this, row, data, 5, 1, 3, ',', '.');\nDTWidget.formatPercentage(this, row, data, 3, 1, 3, ',', '.');\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


- Barplot:

<img src="man/figures/README-unnamed-chunk-11-1.png" width="75%" style="display: block; margin: auto;" />


- Boxplot:

<img src="man/figures/README-unnamed-chunk-12-1.png" width="75%" style="display: block; margin: auto;" />



## Links
Package: [GitHub](https://github.com/dppalomar/portfolioBacktest).

README file: [GitHub-readme](https://github.com/dppalomar/portfolioBacktest/blob/master/README.md).

Vignette: [GitHub-vignette](https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html).

