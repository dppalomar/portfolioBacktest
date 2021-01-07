# this file is to test some examples via package "pbapply"
rm(list = ls())
require(pbapply)
require(parallel)
require(CVXR)

data("dataset10", package = "portfolioBacktest")

portfolio_fun <- function(data) {
  X <- as.matrix(diff(log(data$adjusted))[-1])
  mu <- colMeans(X)
  Sigma <- cov(X)
  for (i in 1:10) {
    w_ <- CVXR::Variable(nrow(Sigma))
    prob <- CVXR::Problem(Minimize(quad_form(w_, Sigma)),
                    constraints = list(w_ >= 0, t(mu) %*% w_ == 1))
    result <- CVXR::solve(prob)
  }
  # return(rnorm(10))
  tmp <- as.vector(result$getValue(w_))
  return(tmp)
  return(as.vector(result$getValue(w_)/sum(result$getValue(w_))))
}


cl <- makeCluster(2, setup_strategy = "sequential")  # see also: https://github.com/rstudio/rstudio/issues/6692
pkgs <- c("CVXR", "xts")
clusterExport(cl = cl, varlist = "pkgs")
clusterEvalQ(cl = cl, expr = sapply(pkgs, function(pkg) require(pkg, character.only = TRUE)))

# clusterEvalQ(cl = cl, expr = eval(parse(text = "library(CVXR)")))
# clusterEvalQ(cl = cl, expr = eval(parse(text = "library(xts)")))

pboptions(type = "timer", style = 6, char = "=")
result <- pblapply(cl = cl, X = dataset10, FUN = portfolio_fun)
stopCluster(cl)
