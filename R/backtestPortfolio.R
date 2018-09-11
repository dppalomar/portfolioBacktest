#' Sparse Index Tracking
#'
#' Computes the weights of assets (relative capital allocation) for a sparse approximation of a financial index.
#'
#' @param X m-by-n matrix of net returns (m samples, n assets).
#' @param r m dimensional vector of the net returns of the index.
#' @param lambda sparsity weight factor. Any nonnegative number (suggested range \code{[10^{-8},10^{-6}]}).
#' @param u upper bound of the weights.Default value \code{u <- 1}, i.e., no effective upper bound.
#' @param measure performance measure. Possible values \code{'ete'} (empirical tracking error - default), \code{'dr'} (downside risk),
#' \code{'hete'} (Huber empirical tracking error), and \code{'hdr'} (Huber downside risk).
#' @param hub Huber parameter. Required if \code{measure = 'hete'} or \code{measure = 'hdr'}.
#' @param w0 initial point. If \code{NULL} a uniform allocation is used, i.e., \code{w0 <- rep(1/N, N)}.
#' @param thres threshold value. All the weights less or equal to \code{thres} are set to 0. The default value is \code{1e-9}.
#' @return An n-dimensional vector with allocation weights on the assets.
#' @author Konstantinos Benidis and Daniel P. Palomar
#' @references
#' K. Benidis, Y. Feng, D. P. Palomar, "Sparse Portfolios for High-Dimensional Financial Index Tracking,"
#' \emph{IEEE Transactions on Signal Processing}, vol. 66, no. 1, pp. 155-170, Jan. 2018.
#' @examples
#' library(sparseIndexTracking)
#' library(xts)
#'
#' # load data
#' data(INDEX_2010)
#'
#' # fit portfolio under error measure ETE (Empirical Tracking Error)
#' w_ete <- spIndexTrack(INDEX_2010$X, INDEX_2010$SP500, lambda = 1e-7, u = 0.5, measure = 'ete')
#'
#' # show cardinality achieved
#' cat("Number of assets used:", sum(w_ete > 1e-6))
#'
#' @export
backtestPortfolio <- function(X, r, lambda, u = 1,
                         measure = c('ete', 'dr', 'hete', 'hdr'),
                         hub = NULL, w0 = NULL, thres = 1e-9) {
  measure <- match.arg(measure)
  max_iter <- 1000 # maximum MM iterations

  ######## error control  #########
  X <- as.matrix(X)
  n <- ncol(X)
  m <- nrow(X)
  if (n == 1) stop("Data is univariate!")
  if (anyNA(X) || anyNA(r) || anyNA(lambda) || anyNA(u)) stop("This function cannot handle NAs.")
  if (((measure == 'hete') || (measure == 'hdr')) && ((is.null(hub)) || (hub <= 0))) stop("The input argument 'hub' should be positive.")
  if (u <= 0) stop("The input argument 'u' should be positive.")
  #################################

  if (is.null(w0))
    w0 <- rep(1/n, n)
  else
    if (anyNA(w0)) stop("This function cannot handle NAs.")

  # Preallocation
  F_v <- matrix(0, max_iter, 1)  # objective value

  # Decreasing p
  K <- 10  # number of outer iterations
  p1 <- 1  # first value of -log(p)
  pk <- 7  # last value of -log(p)
  gamma <- (pk/p1)^(1/K)
  pp <- p1 * gamma^(0:K)
  pp <- 10^(-pp)

  tol <- pmin(pp/10, 1e-3)  # tolerance for convergence

  k <- 0  # MM iteration counter

  ######################### MM LOOP FOR ETE #########################
  if (measure == 'ete') {
    A <- 1/m * t(X) %*% X
    Lmax_A <- eigen(A, symmetric = TRUE, only.values = TRUE)$values[1]

    B <- 2/Lmax_A * (A - Lmax_A*diag(n))
    b <- -2/m * t(X) %*% r

    for (ee in 1:(K+1)) {  # loop for approximation based on p & epsilon
      p <- pp[ee]
      c1 <- log(1 + u/p)
      flg <- 1

      while (1) {
        k <- k + 1

        # Acceleration double step
        w1 <- eteMMupdate(w0, B, b, Lmax_A, lambda, p, c1, u)
        w2 <- eteMMupdate(w1, B, b, Lmax_A, lambda, p, c1, u)
        R <- w1 - w0
        U <- w2 - w1 - R
        a <- max(min(-norm(R, type = "2") / norm(U, type = "2"), -1), -300)

        # backtracking loop to ensure feasibility
        while (1) {
          if (abs(a+1) < 1e-6) {
            w <- w2
            F_v[k] <- 1/lambda * norm(X %*% w - r, type = "2")^2 + m/c1 * sum(log(1 + w/p))

            w0 <- w
            break
          }

          w <- w0 - 2*a*R + a^2*U

          # Projection
          w <- bisection(-2*w, u)
          F_v[k] <- 1/lambda * norm(X %*% w - r, type = "2")^2 + m/c1 * sum(log(1 + w/p))

          if (flg == 0 && F_v[k] * (1 - sign(F_v[k])*1e-9) >= F_v[max(k - 1, 1)])
            a <- (a-1)/2
          else {
            w0 <- w
            break
          }
        }

        # Stopping criterion
        if (flg == 0) {
          rel_change <- (abs(F_v[k] - F_v[k - 1]) / max(1, abs(F_v[k - 1]) ) ) # relative change in objective
          if ((rel_change <= tol[ee]) || (k >= max_iter))
            break
        }
        flg <- 0
      }
    }
  }


  ######################### MM LOOP FOR DR #########################
  else if (measure == 'dr') {
    A <- 1/m * t(X) %*% X
    Lmax_A <- eigen(A, symmetric = TRUE, only.values = TRUE)$values[1]

    B <- 2/Lmax_A * (A - Lmax_A*diag(n))
    b <- -2/m * t(X) %*% r

    for (ee in 1:(K+1)) {  # loop for approximation based on p & epsilon
      p <- pp[ee]
      c1 <- log(1 + u/p)
      flg <- 1

      while (1) {
        k <- k + 1

        # Acceleration double step
        w1 <- drMMupdate(w0, X, r, B, b, Lmax_A, lambda, p, c1, u, m)
        w2 <- drMMupdate(w1, X, r, B, b, Lmax_A, lambda, p, c1, u, m)
        R <- w1 - w0
        U <- w2 - w1 - R
        a <- max(min(-norm(R, type = "2") / norm(U, type = "2"), -1), -300)

        # backtracking loop to ensure feasibility
        while (1) {
          if (abs(a+1) < 1e-6) {
            w <- w2
            F_v[k] <- 1/lambda * norm(pmax(r - X %*% w, 0), type = "2")^2 + m/c1 * sum(log(1 + w/p))

            w0 <- w
            break
          }

          w <- w0 - 2*a*R + a^2*U

          # Projection
          w <- bisection(-2*w, u)
          F_v[k] <- 1/lambda * norm(pmax(r - X %*% w, 0), type = "2")^2 + m/c1 * sum(log(1 + w/p))

          if (flg == 0 && F_v[k] * (1 - sign(F_v[k])*1e-9) >= F_v[max(k - 1, 1)])
            a <- (a-1)/2
          else {
            w0 <- w
            break
          }
        }

        # Stopping criterion
        if (flg == 0) {
          rel_change <- (abs(F_v[k] - F_v[k - 1]) / max(1, abs(F_v[k - 1]) ) ) # relative change in objective
          if ((rel_change <= tol[ee]) || (k >= max_iter)) {
            break
          }
        }
        flg <- 0
      }
    }
  }


  ######################### MM LOOP FOR HETE #########################
  else if (measure == 'hete') {

    for (ee in 1:(K+1)) {  # loop for approximation based on p & epsilon
      p <- pp[ee]
      c1 <- log(1 + u/p)
      flg <- 1

      while (1) {
        k <- k + 1

        # Acceleration double step(w, X, r, lambda, p, c1, m, n, hub, u)
        w1 <- heteMMupdate(w0, X, r, lambda, p, c1, m, n, hub, u)
        w2 <- heteMMupdate(w1, X, r, lambda, p, c1, m, n, hub, u)
        R <- w1 - w0
        U <- w2 - w1 - R
        a <- max(min(-norm(R, type = "2") / norm(U, type = "2"), -1), -300)

        # backtracking loop to ensure feasibility
        while (1) {
          if (abs(a+1) < 1e-6) {
            w <- w2
            tmp <- r - X %*% w
            h <- rep(0, m)
            h[abs(tmp) <= hub] <- (tmp[abs(tmp) <= hub])^2
            h[abs(tmp) > hub] <- hub * (2*abs(tmp[abs(tmp) > hub]) - hub)

            F_v[k] <- 1/lambda * sum(h) + m/c1 * sum(log(1 + w/p))

            w0 <- w
            break
          }

          w <- w0 - 2*a*R + a^2*U

          # Projection
          w <- bisection(-2*w, u)
          tmp <- r - X %*% w
          h <- rep(0, m)
          h[abs(tmp) <= hub] <- (tmp[abs(tmp) <= hub])^2
          h[abs(tmp) > hub] <- hub * (2*abs(tmp[abs(tmp) > hub]) - hub)

          F_v[k] <- 1/lambda * sum(h) + m/c1 * sum(log(1 + w/p))

          if ((flg == 0) && ((F_v[k] * (1 - sign(F_v[k])*1e-9)) >= F_v[max(k - 1, 1)]))
            a <- (a-1)/2
          else {
            w0 <- w
            break
          }
        }

        # Stopping criterion
        if (flg == 0) {
          rel_change <- (abs(F_v[k] - F_v[k - 1]) / max(1, abs(F_v[k - 1]) ) ) # relative change in objective
          if ((rel_change <= tol[ee]) || (k >= max_iter)) {
            break
          }
        }

        flg <- 0
      }
    }
  }

  ######################### MM LOOP FOR HDR #########################
  else if (measure == 'hdr') {

    for (ee in 1:(K+1)) {  # loop for approximation based on p & epsilon
      p <- pp[ee]
      c1 <- log(1 + u/p)
      flg <- 1

      while (1) {
        k <- k + 1

        # Acceleration double step
        w1 <- hdrMMupdate(w0, X, r, lambda, p, c1, m, n, hub, u)
        w2 <- hdrMMupdate(w1, X, r, lambda, p, c1, m, n, hub, u)
        R <- w1 - w0
        U <- w2 - w1 - R
        a <- max(min(-norm(R, type = "2") / norm(U, type = "2"), -1), -300)

        # backtracking loop to ensure feasibility
        while (1) {
          if (abs(a+1) < 1e-6) {
            w <- w2
            tmp <- r - X %*% w
            h <- rep(0, m)
            h[(tmp > 0) && (tmp <= hub)] <- tmp[(tmp > 0) && (tmp <= hub)]^2
            h[tmp > hub] <- hub * (2*abs(tmp[tmp > hub]) - hub)

            F_v[k] <- 1/lambda * sum(h) + m/c1 * sum(log(1 + w/p))

            w0 <- w
            break
          }

          w <- w0 - 2*a*R + a^2*U

          # Projection
          w <- bisection(-2*w, u)
          tmp <- r - X %*% w
          h <- rep(0, m)
          h[(tmp > 0) && (tmp <= hub)] <- tmp[(tmp > 0) && (tmp <= hub)]^2
          h[tmp > hub] <- hub * (2*abs(tmp[tmp > hub]) - hub)

          F_v[k] <- 1/lambda * sum(h) + m/c1 * sum(log(1 + w/p))

          if (flg == 0 && F_v[k] * (1 - sign(F_v[k])*1e-9) >= F_v[max(k - 1, 1)])
            a <- (a-1)/2
          else {
            w0 <- w
            break
          }
        }

        # Stopping criterion
        if (flg == 0) {
          rel_change <- (abs(F_v[k] - F_v[k - 1]) / max(1, abs(F_v[k - 1]) ) ) # relative change in objective
          if ((rel_change <= tol[ee]) || (k >= max_iter)) {
            break
          }
        }
        flg <- 0
      }
    }
  }

  # Threshold
  w[w < thres] <- 0
  w <- w / sum(w)

  # name
  if (!is.null(colnames(X)))
    names(w) <- colnames(X)

  return(w)
}



# ete MM update at each iteration
eteMMupdate <- function(w, B, b, Lmax_A, lambda, p, c1, u) {
  d <- lambda / ((p + abs(w)) * c1)
  c <- B %*% w + 1/Lmax_A * (b + d)

  return (bisection(c, u))
}


# dr MM update at each iteration
drMMupdate <- function(w, X, r, B, b, Lmax_A, lambda, p, c1, u, m) {
  h <- pmin(r - X %*% w, 0)
  d <- lambda / ((p + abs(w)) * c1)
  c <- B %*% w + 1/Lmax_A * (b + d + 2/m * t(X) %*% h)

  return (bisection(c, u))
}


# hete MM update at each iteration
heteMMupdate <- function(w, X, r, lambda, p, c1, m, n, hub, u) {
  d <- lambda / ((p + abs(w)) * c1)
  tmp <- r - X %*% w
  alpha <- rep(1, m)
  alpha[abs(tmp) > hub] <- hub / abs(tmp[abs(tmp) > hub])

  Q <- 1/m * t(X) %*% diag(alpha) %*% X
  Lmax <- eigen(Q, symmetric = TRUE, only.values = TRUE)$values[1]

  c <- 1/Lmax * (2*(Q - Lmax*diag(rep(1, n))) %*% w - 2/m*t(X) %*% diag(alpha) %*% r + d)

  return (bisection(c, u))
}


# hdr MM update at each iteration
hdrMMupdate <- function(w, X, r, lambda, p, c1, m, n, hub, u) {
  d <- lambda / ((p + abs(w)) * c1)

  tmp <- r - X %*% w
  alpha <- rep(1, m)
  alpha[tmp > hub] <- hub / tmp[tmp > hub]
  alpha[tmp < 0] <- hub / (hub - 2*tmp[tmp < 0])

  q <- -pmax(X %*% w - r, 0)

  Q <- 1/m * t(X) %*% diag(alpha) %*% X
  Lmax <- eigen(Q, symmetric = TRUE, only.values = TRUE)$values[1]

  c <- 1/Lmax * (2*(Q - Lmax*diag(rep(1, n))) %*% w + 2/m*t(X) %*% diag(alpha) %*% (q - r) + d)

  return (bisection(c, u))
}


# bisection algorithm that solves the KKT
bisection <- function(c, u) {
  n <- length(c)
  w <- rep(0, n)

  sort_ind <- order(c)
  c_sort <- c[sort_ind]

  high <- n
  low <- 1

  while (low <= high) {
    mid <- floor((low + high) / 2)
    mu <- -1/mid * (sum(c_sort[1:mid]) + 2)

    tst1 <- (mu + c_sort[mid] < 0)
    if (mid < n) {
      tst2 <- (mu + c_sort[mid+1] >= 0)
    }
    else {
      tst2 <- TRUE
    }

    if (tst1 && tst2) {
      break
    }
    else if (tst1 && !tst2) {
      low <- mid + 1
    }
    else {
      high <- mid - 1
    }
  }

  if (all(-(mu + c_sort[1:mid])/2 <= u)) {
    w[sort_ind[1:mid]] <- -(mu + c_sort[1:mid])/2
    return (w)
  }
  else {
    flg <- FALSE
    flg2 <- FALSE
    k <- mid

    while (1) {
      low1 <- 0
      high1 <- k - 1

      while (low1 <= high1) {
        mid1 <- floor((low1 + high1)/2)
        mu <- (2*mid1*u - sum(c_sort[(mid1+1):k]) - 2) / (k - mid1)

        if (mid1 != 0) {
          tst1 <- (mu + c_sort[mid1] <= -2*u)
        }
        else {
          tst1 <- TRUE
        }

        tst2 <- ((-2*u) < (mu + c_sort[mid1+1])) && ((mu + c_sort[k]) < 0)

        if (k < n) {
          tst3 <- ((mu + c_sort[k+1]) >= 0)
        }
        else {
          tst3 <- TRUE
        }

        if (tst1 && tst2 && tst3) {
          flg <- TRUE
          break
        }
        else if (tst1 && !tst2) {
          low1 <- mid1 + 1
        }
        else {
          high1 <- mid1 - 1
        }
      }

      if (flg) {
        break
      }
      else {
        k <- k + 1
      }

      if (k > n) {
        flg2 <- TRUE
        break
      }
    }

    if (flg2) {
      w[sort_ind[1:ceiling(1/u)]] <- u
      return (w)
    }
    else {
      w[sort_ind[1:mid1]] <- u
      w[sort_ind[(mid1+1):k]] <- -(mu + c_sort[(mid1+1):k])/2
      return (w)
    }
  }
}


