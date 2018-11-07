parallel::detectCores()

library(portfolioBacktest)
library(PerformanceAnalytics)
library(CVXR)
data(prices, package = "portfolioBacktest")

# define Markowitz portfolio functions
portfolio_fun_Markowitz <- function(prices) {
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

portfolio_function_list <- c(GMVP_portfolio_fun, uniform_portfolio_fun, portfolio_fun_Markowitz)


# parallel testing for function "portfolioBacktest()"
system.time(
res <- portfolioBacktest(portfolio_fun = portfolio_fun_Markowitz, prices = prices[1:9],
                         par_dataset = 3, packages = "CVXR")
)

system.time(
res_signel <- portfolioBacktest(portfolio_fun = portfolio_fun_Markowitz, prices = prices[1:9])
)
# parallel testing for function "multiplePortfolioBacktest()"
my_path <- "d:/Users/rzhouae/Documents/R/Git/portfolioBacktest/R_buildignore/student-functions/"

res <- multiplePortfolioBacktest(folder_path = my_path, prices = prices[1:3], par_strategy = 3)
res_signel <- multiplePortfolioBacktest(folder_path = my_path, prices = prices[1:3], par_strategy = 1, par_dataset = 3)
str(res)
str(res_signel)

# parallel testing for function "multiplePortfolioBacktest()" using a list of functions
res <- multiplePortfolioBacktest(portfolio_fun_list = portfolio_function_list, 
                                 prices = prices[1:3], shortselling = TRUE, par_strategy = 3, packages = "CVXR")
str(res)

# parallel at both stage
res <- multiplePortfolioBacktest(folder_path = my_path, prices = prices[1:3], par_strategy = 3, par_dataset = 3)


# sanity check
f1 <- function(prices) {N <- f2(prices); return(rep(1/N, N))}
f2 <- function(prices) {return(ncol(prices))}
res1 <- portfolioBacktest(portfolio_fun = f1, prices = prices[1:3],
                          par_dataset = 3, packages = "CVXR")
res1$error_message

res2 <- portfolioBacktest(portfolio_fun = f1, prices = prices[1:3],
                          par_dataset = 3, packages = "CVXR", assist_funs = c("f2"))
res2$error_message