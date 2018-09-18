<<<<<<< HEAD
library(CVXR)

portfolio_fun <- function(data) {
  mu <- colMeans(data)
  Sigma <- cov(data)
  lmd = 0.5
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - lmd*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}
||||||| merged common ancestors
=======
library(CVXR)

portfolio_fun <- function(data) {
  mu <- colMeans(data)
  Sigma <- cov(data)
  lmd = 0.5
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - lmd*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}
>>>>>>> 2ce7faa36dfed1b3181cdd8b32c1d33cc462c1a0
