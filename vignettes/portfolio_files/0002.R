library(CVXR)

# Markowitz mean-variance
portfolio_fun <- function(data) {
  X <- as.matrix(diff(log(data$adjusted))[-1])
  mu <- colMeans(X)
  Sigma <- cov(X)
  lmd = 0.5
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - lmd*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}
