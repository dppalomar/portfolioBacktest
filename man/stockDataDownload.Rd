% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stockDataDownload.R
\name{stockDataDownload}
\alias{stockDataDownload}
\title{Stocks data download from Internet}
\usage{
stockDataDownload(stock_symbols, index_symbol = NULL,
  check_monotone = TRUE, from, to, ...)
}
\arguments{
\item{stock_symbols}{a string vector containing symbols of stocks to be downloaded.
User can pass the market index symbol as its attribute "index_symbol" 
(only considered when argument `index_symbol` is not passed).}

\item{index_symbol}{a string as the market index symbol of stocks included in `stock_symbols`.}

\item{check_monotone}{a logical value indicating whether to check and delete data not satisfying monotone missing pattern.}

\item{from}{a string as the starting date, e.g., "2017-08-17".}

\item{to}{a string as the ending date (not included), e.g., "2017-09-17".}

\item{...}{additional arguments to be passed to `quantmod::getSymbols()``}
}
\value{
a list of 7 xts objects which are `open`, `high`, `low`, `close`, `volume`, `adjusted` and `index`.
        Note that `index` will only be returned when correct index symbols is passed.
}
\description{
Use package "quantmod" to download stocks prices data.
}
\author{
Rui Zhou and Daniel P. Palomar
}