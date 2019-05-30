library(CVXR)

# Maximum Sharpe ratio
portfolio_fun <- function(data) {
  X <- as.matrix(diff(log(data$adjusted))[-1])
  mu <- colMeans(X)
  Sigma <- cov(X)
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w_)/sum(result$getValue(w_))))
}
