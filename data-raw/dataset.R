
#generate heavy-tailed data and save it
N <- 40
T <- 1000
mu <- rep(0,N)
nv <- 4
U <- t(mvtnorm::rmvnorm(n=round(0.7*N), sigma=0.1*diag(N)))
R_cov <- U %*% t(U) + diag(N)
X <- mvtnorm::rmvt(n=T, delta=mu, sigma=(nv-2)/nv*R_cov, df=nv)

heavy_data <- list(N = N,
                   T = T,
                   mu = mu,
                   cov = R_cov,
                   nv = nv,
                   X = X)

devtools::use_data(heavy_data, overwrite = TRUE)
