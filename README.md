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

For a more detailed explanation on how to use the package with all the features, check the [vignette](https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html).



## Package Snapshot
This package backtests a list of portfolios over multiple datasets on a rolling-window basis, producing final results as in the following.

- Performance table:

<img src="man/figures/README-table.png" width="85%" style="display: block; margin: auto;" />


- Barplot:

<img src="man/figures/README-barplot.png" width="70%" style="display: block; margin: auto;" />


- Boxplot:

<img src="man/figures/README-boxplot.png" width="65%" style="display: block; margin: auto;" />



## Links
Package: [CRAN](https://CRAN.R-project.org/package=portfolioBacktest) and [GitHub](https://github.com/dppalomar/portfolioBacktest).

README file: [GitHub-readme](https://github.com/dppalomar/portfolioBacktest/blob/master/README.md).

Vignette: [CRAN-vignette](https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html).

