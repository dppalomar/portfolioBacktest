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

# Citing this work
citation("backtestPortfolio")
```

Usage of `backtestPortfolio()`
------------------------------

We start by loading the package and some market data from the underlying assets of the S&P 500:

``` r
library(backtestPortfolio)
library(xts)
data(prices1)
```

The data `prices1` contains the log-prices of a randomly selected sample of *N* = 30 stocks from the S&P 500 (the package also contains four other sets or randomly selected data, namely, `prices2`, `prices3`, `prices4`, and `prices5`).

``` r
head(prices1)
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

We are then ready to use the function `backtestPortfolio()`:

``` r
res <- backtestPortfolio(naivePortfolioDesign, prices1)
print(res)
```

Links
-----

Package: [GitHub](https://github.com/dppalomar/backtestPortfolio).

README file: [GitHub-readme](https://rawgit.com/dppalomar/backtestPortfolio/master/README.html).
