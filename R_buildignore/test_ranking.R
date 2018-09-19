# generate fake results
N <- 100
sharpe_ratio <- rnorm(N)
failure_rate <- sample(c(rep(0, N/2), runif(N/2)))


# 1) convert to scores (higher the better)
sharpe_ratio_score <- sharpe_ratio
failure_rate_score <- -failure_rate
hist(sharpe_ratio_score, breaks=20, prob=TRUE, col="lightgray", xlab="value", main="Histogram")
hist(failure_rate_score, breaks=20, prob=TRUE, col="lightgray", xlab="value", main="Histogram")


# 2) compute percentiles
ecdf_sharpe_ratio <- ecdf(sharpe_ratio_score)
percentile_sharpe_ratio <- ecdf_sharpe_ratio(sharpe_ratio_score)
#plot(sort(percentile_sharpe_ratio))
ecdf_failure_rate <- ecdf(failure_rate_score)
percentile_failure_rate <- ecdf_failure_rate(failure_rate_score)
#plot(sort(percentile_failure_rate))


# 3) compute weighted average as overall performance
weights <- c(10, 90)
percentile_all <- cbind(percentile_sharpe_ratio, percentile_failure_rate)
overall_performance <- as.vector(percentile_all %*% weights)


# 4) rank based on overall performance
plot(failure_rate, overall_performance)
plot(sharpe_ratio, overall_performance)
