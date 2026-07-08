#' Download FXMacroData release-calendar events
#'
#' Fetch official-source macroeconomic release events from FXMacroData and
#' return an \code{xts} object that can be joined with portfolio price windows.
#'
#' @param currency ISO currency code, for example \code{"usd"}.
#' @param limit Maximum number of events to request.
#' @param min_tier Optional market-tier filter. Use \code{1} for top-tier
#'   events, \code{2} for medium-or-higher impact, or \code{NULL} for all rows.
#' @param api_key Optional FXMacroData API key. Defaults to the
#'   \code{FXMACRODATA_API_KEY} environment variable.
#' @param base_url FXMacroData REST API base URL.
#'
#' @return An \code{xts} object indexed by release date with market tier and
#'   top-tier flags.
#' @export
fxmacrodataCalendar <- function(currency = "usd",
                                limit = 100,
                                min_tier = 2,
                                api_key = Sys.getenv("FXMACRODATA_API_KEY"),
                                base_url = "https://fxmacrodata.com/api/v1") {
  limit <- max(1L, min(as.integer(limit), 100L))
  params <- paste0("?limit=", limit)
  if (nzchar(api_key))
    params <- paste0(params, "&api_key=", utils::URLencode(api_key, reserved = TRUE))

  url <- paste0(
    sub("/$", "", base_url),
    "/calendar/",
    tolower(currency),
    params
  )

  payload <- jsonlite::fromJSON(url)
  events <- payload$data
  if (is.null(events) || NROW(events) == 0L)
    return(xts::xts())

  if (!is.null(min_tier) && "market_tier" %in% names(events))
    events <- events[events$market_tier <= min_tier, , drop = FALSE]

  if (NROW(events) == 0L)
    return(xts::xts())

  events <- utils::head(events, limit)
  idx <- as.Date(events$date)
  values <- data.frame(
    market_tier = events$market_tier,
    top_tier_for_currency = events$top_tier_for_currency,
    release = events$release,
    name = events$name
  )
  xts::xts(values, order.by = idx)
}
