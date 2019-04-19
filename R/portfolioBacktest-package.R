#' portfolioBacktest: Automatic Backtesting of Portfolios Over Multiple Datasets
#'
# 'Backtesting of a portfolio in a sliding-window fashion over a 
# 'dataset of stock prices. Multiple datasets are allowed (e.g., taken 
# 'randomly over different markets, different time periods, and different
# 'subset of the stock universe). In addition, multiple portfolios can be
# 'backtested for a subsequent comparison and ranking on a number of 
# 'criteria including expected return, volatility, Sharpe ratio, maximum 
# 'drawdown, turnover rate, return on investment, computational time, etc.
# 'The portfolio is defined as a function that takes as input a window of 
# 'the stock prices and outputs the portfolio weights. This package can be 
# 'useful for a researcher/practitioner who wants to backtest a set of 
# 'portfolios over a multitude of datasets over different markets. In 
# 'addition, it can be particularly useful to evaluate students in a 
# 'portfolio design course where the grading is based on the performance.

#' @section Functions:
#' \code{\link{portfolioBacktest}}, \code{\link{multiplePortfolioBacktest}}
#'
#' @section Help:
#' For a quick help see the README file:
#' \href{https://CRAN.R-project.org/package=portfolioBacktest/readme/README.html}{CRAN-README} and
#' \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/README.html}{GitHub-README}.
#' 
#' For more details see the vignette:
#' \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest.html}{CRAN-html-vignette},
#' \href{https://CRAN.R-project.org/package=portfolioBacktest/vignettes/PortfolioBacktest-pdf.pdf}{CRAN-pdf-vignette},
#' \href{https://raw.githack.com/dppalomar/portfolioBacktest/master/vignettes/PortfolioBacktest.html}{GitHub-html-vignette}, and
#' \href{https://docs.google.com/viewer?url=https://github.com/dppalomar/portfolioBacktest/raw/master/vignettes/PortfolioBacktest-pdf.pdf}{GitHub-pdf-vignette}.
#'
#' @author Rui ZHOU and Daniel P. Palomar
#'
#' @docType package
#' @name portfolioBacktest-package
NULL
