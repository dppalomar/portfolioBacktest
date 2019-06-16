library(CVXR)

# GMVP
portfolio_fun <- function(data) {
  X <- as.matrix(diff(log(data$adjusted))[-1])
  Sigma <- cov(X)
  w <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w, Sigma)), 
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

