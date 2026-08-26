\name{fxmacrodataCalendar}
\alias{fxmacrodataCalendar}
\title{Download FXMacroData release-calendar events}
\usage{
fxmacrodataCalendar(currency = "usd", limit = 100, min_tier = 2,
  api_key = Sys.getenv("FXMACRODATA_API_KEY"),
  base_url = "https://api.fxmacrodata.com/v1")
}
\arguments{
  \item{currency}{ISO currency code, for example \code{"usd"}.}
  \item{limit}{Maximum number of events to request.}
  \item{min_tier}{Optional market-tier filter. Use \code{1} for top-tier
  events, \code{2} for medium-or-higher impact, or \code{NULL} for all rows.}
  \item{api_key}{Optional FXMacroData API key. Defaults to the
  \code{FXMACRODATA_API_KEY} environment variable.}
  \item{base_url}{FXMacroData REST API base URL.}
}
\description{
Fetch official-source macroeconomic release events from FXMacroData and return
an \code{xts} object that can be joined with portfolio price windows.
}
\value{
An \code{xts} object indexed by release date with market tier and top-tier
flags.
}
\examples{
\dontrun{
events <- fxmacrodataCalendar("usd", min_tier = 1)
}
}
