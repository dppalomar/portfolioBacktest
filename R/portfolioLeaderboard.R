#' @title The leaderboard of portfolio function or students
#' 
#' @description Ranking the portfolio functions according to passed criteria
#' 
#' @param res Returned result from function \code{multiplePortfolioBacktest}
#' @param weights a list of weights for \code{Sharpe ratio}, \code{max drawdown}, \code{expected return}, \code{volatility} 
#'                \code{Sterling ratio}, \code{Omega ratio}, \code{ROT}, \code{cpu time} and \code{failure ratio}
#'
#' @return a matrix as the leaderboard
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @export
portfolioLeaderboard <- function(res = NA, weights = list(), summary_fun = median, show_benchmark = TRUE) {
  if (!is.list(weights)) stop("argument \"weights\" must be a list")
  if (any(unlist(weights) < 0)) stop("all weights must be non-negative")
  if (all(unlist(weights) == 0)) stop("cannot set all weights be zero")
  
  tmp <- backtestSummary(res = res, summary_funs = list(summary_fun), show_benchmark = show_benchmark)
  performance_summary <- tmp[[1]]
  failure_ratio       <- tmp$failure_rate
  cpu_time_average    <- tmp$cpu_time_average
  error_message       <- tmp$error_message
  
  weights_default <- list('Sharpe ratio' = 0, 'max drawdown' = 0, 'annual return' = 0, 'annual volatility' = 0,
                          'Sterling ratio' = 0, 'Omega ratio' = 0, 'ROT bps' = 0, 'cpu time' = 0, 'failure rate' = 0)
  weights_comb <- modifyList(weights_default, weights)
  if (length(weights_comb) != length(weights_default)) stop("contain invalid elements in \"weights\"")
  
  weights_comb <- unlist(weights_comb)
  mask_criteria <- weights_comb > 0
  
  # sort the vaild scores
  weights_rescaled <- weights_comb / sum(weights_comb)
  mask_valid <- failure_ratio != 1
  scores <- cbind(rank_percentile( performance_summary[mask_valid, 1]),
                  rank_percentile(-performance_summary[mask_valid, 2]),
                  rank_percentile( performance_summary[mask_valid, 3]),
                  rank_percentile(-performance_summary[mask_valid, 4]),
                  rank_percentile( performance_summary[mask_valid, 5]),
                  rank_percentile( performance_summary[mask_valid, 6]),
                  rank_percentile( performance_summary[mask_valid, 7]),
                  rank_percentile(-cpu_time_average[mask_valid]),
                  rank_percentile(-failure_ratio[mask_valid]))
  final_score <- scores %*% weights_rescaled
  index_sorting <- sort(final_score, decreasing = TRUE, index = TRUE)$ix
  
  # combine the valid and invalid scores
  leaderboard_valid <- cbind(scores[index_sorting, ], final_score[index_sorting])
  leaderboard_invalid <- matrix(NA, sum(!mask_valid), length(weights_comb) + 1)
  leaderboard <- rbind(leaderboard_valid, leaderboard_invalid)
  
  # add names
  index_vaild_sorted <- (1:length(mask_valid))[mask_valid][index_sorting]
  index_sorted <- c(index_vaild_sorted, (1:length(mask_valid))[-index_vaild_sorted])

  # also show original performance
  error_summary <- error_message[index_sorted]
  leaderboard_performance <- cbind(performance_summary,
                                   cpu_time_average,
                                   failure_ratio)[index_sorted, ]
  
  # add rownames and colnames
  rownames(leaderboard) <- rownames(leaderboard_performance) <- names(error_message)[index_sorted]
  colnames(leaderboard) <- paste(c(names(weights_default), 'final'), 'score')
  colnames(leaderboard_performance) <- names(weights_default)
  
  # return 
  return(list("leaderboard_scores" = leaderboard[, mask_criteria],
              "leaderboard_performance" = leaderboard_performance,
              "error_summary" = error_summary))
}

rank_percentile <- function(x) {
  N <- length(x)
  rank_pctl <- ecdf(x)(x)
  rank_pctl <- (rank_pctl - 1/N)/(1 - 1/N)
  return (100*rank_pctl)
}
  
