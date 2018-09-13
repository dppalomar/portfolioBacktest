library(CVXR)
library(MASS)

portfolio_fun <- function(data) {
  mu <- colMeans(data)
  Sigma <- cov(data)
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- solve(prob)
  tmp <- mvrnorm(1, 1, 1)
  mu <- (mu + tmp) - tmp
  return(as.vector(result$getValue(w_)/sum(result$getValue(w_))))
}