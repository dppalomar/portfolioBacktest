parallel::detectCores()

library(portfolioBacktest)
library(CVXR)
data("dataset")

# define Markowitz portfolio functions
portfolio_fun_Markowitz <- function(prices) {
  require(CVXR)
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

uniform_portfolio_fun <- function(prices) {
  N <- ncol(prices)
  w <- rep(1/N, N)  # satisfies the constraints w>=0 amd sum(w)=1
  return(w)
}

GMVP_portfolio_fun <- function(prices) {
  X <- diff(log(prices))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- w/sum(abs(w))  # satisfies sum(w)=1 but not necessarily w>=0
  return(w)
}

portfolio_function_list <- c(uniform_portfolio_fun, GMVP_portfolio_fun, portfolio_fun_Markowitz)


# parallel testing for function "portfolioBacktest()"
system.time(
  res <- portfolioBacktest(portfolio_funs = portfolio_fun_Markowitz, dataset = dataset, par_dataset = 3)
)

system.time(
  res_signel <- portfolioBacktest(portfolio_funs = portfolio_fun_Markowitz, dataset = dataset)
)

system.time(
  res_market <- portfolioBacktest(portfolio_funs = portfolio_fun_Markowitz, dataset = dataset, market = TRUE, par_dataset = 3)
)


sapply(res[[1]], function(x){x$performance})
sapply(res[[1]], function(x){x$benchmark$uniform$performance})
sapply(res_signel[[1]], function(x){x$performance})
sapply(res_signel[[1]], function(x){x$benchmark$uniform$performance})

# parallel testing for function "multiplePortfolioBacktest()"
my_path <- "d:/Users/rzhouae/Documents/R/Git/portfolioBacktest/R_buildignore/student-functions/"
my_mac_path <- "/Users/zhourui/Documents/R/GitProjects/portfolioBacktest/R_buildignore/student-functions/"

res <- portfolioBacktest(folder_path = my_path, dataset = dataset, par_portfolio = 3)
res_single <- portfolioBacktest(folder_path = my_path, dataset = dataset, par_portfolio = 1)
sapply(res[[3]], function(x){x$performance})
res[[3]][[1]]$error_message
sapply(res_single[[3]], function(x){x$performance})
res_single[[3]][[1]]$error_message

# parallel testing for function "multiplePortfolioBacktest()" using a list of functions
res_par <- portfolioBacktest(portfolio_funs = portfolio_function_list,
                             dataset = dataset, shortselling = TRUE, par_portfolio = 3)

res_sig <- portfolioBacktest(portfolio_funs = portfolio_function_list,
                             dataset = dataset, shortselling = TRUE, par_portfolio = 1)
sapply(res_par[[3]], function(x){x$performance})
sapply(res_sig[[3]], function(x){x$performance})

# parallel at both stage (not recommend)
res <- portfolioBacktest(folder_path = my_path, dataset = dataset, par_portfolio = 3, par_dataset = 3, packages = "CVXR")
sapply(res[[3]], function(x){x$performance})
sapply(res_single[[3]], function(x){x$performance})

# sanity check
f1 <- function(prices) {N <- f2(prices); return(rep(1/N, N))}
f2 <- function(prices) {return(ncol(prices))}
res1 <- portfolioBacktest(portfolio_funs = f1, dataset =  dataset,
                          par_dataset = 3, packages = "CVXR")
res1[[1]][[1]]$error_message

res2 <- portfolioBacktest(portfolio_fun = f1, dataset =  dataset,
                          par_dataset = 3, packages = "CVXR", assist_funs = c("f2"))
res2[[1]][[1]]$error_message
