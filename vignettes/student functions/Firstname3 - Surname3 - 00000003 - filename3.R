<<<<<<< HEAD
library(CVXR)

portfolio_fun <- function(data) {
  mu <- colMeans(data)
  Sigma <- cov(data)
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w_)/sum(result$getValue(w_))))
}
||||||| merged common ancestors
=======
library(CVXR)

portfolio_fun <- function(data) {
  mu <- colMeans(data)
  Sigma <- cov(data)
  w_ <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w_, Sigma)),
                  constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w_)/sum(result$getValue(w_))))
}
>>>>>>> 2ce7faa36dfed1b3181cdd8b32c1d33cc462c1a0
