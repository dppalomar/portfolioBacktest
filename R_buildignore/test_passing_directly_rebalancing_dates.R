


# indices
#rebalancing_indices <- endpoints(prices, on = "weeks")[which(endpoints(prices, on = "weeks") >= T_rolling_window)]
optimize_indices <- seq(from = T_rolling_window, to = T, by = optimize_every)
rebalance_indices <- seq(from = T_rolling_window, to = T, by = rebalance_every)
# rebalancing_dates <- as.Date(c("2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31",
#                                "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31",
#                                "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30",
#                                "2018-09-30", "2018-12-31", "2019-03-31", "2019-06-30"))
# rebalance_indices <- optimize_indices <- which(index(prices) %in% rebalancing_dates)