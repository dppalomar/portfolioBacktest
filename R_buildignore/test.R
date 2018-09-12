library(CVXR)

naivePortfolioDesign <- function(prices) {
  # compute log returns
  X <- diff(log(prices))[-1]
  
  # compute mean vector and SCM
  mu <- colMeans(X)
  Sigma <- cov(X)
  
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}