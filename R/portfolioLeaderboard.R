#' @title The leaderboard of portfolio function or students
#' 
#' @description Ranking the portfolio functions according to passed criteria
#' 
#' @param res Returned result from function \code{multiplePortfolioBacktest}
#' @param weights weights for \code{sharpe ratio}, \code{max drawdown}, \code{expected return} 
#'                \code{cpu time} and \code{failure ratio}
#'
#' @return a matrix as the leaderboard
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @export
portfolioLeaderboard <- function(res = NA, weights = c(1, 1, 1, 1, 6)) {
  if (length(weights) != 5) stop("argument \"weights\" must have 5 elements")
  
  # sort the vaild scores
  weights <- weights / (sum(weights))
  mask_valid <- res$failure_ratio != 1
  scores <- cbind(rank_percentile(res$performance_summary[mask_valid, 1]),
                  rank_percentile(-res$performance_summary[mask_valid, 2]),
                  rank_percentile(-res$performance_summary[mask_valid, 3]),
                  rank_percentile(-res$cpu_time_average[mask_valid]),
                  rank_percentile(-res$failure_ratio[mask_valid]))
  final_score <- scores %*% weights
  index_sorting <- sort(final_score, decreasing = TRUE, index = TRUE)$ix
  
  # combine the valid and invalid scores
  leaderboard_valid <- cbind(scores[index_sorting, ], final_score[index_sorting])
  leaderboard_invalid <- matrix(NA, sum(!mask_valid), 6)
  leaderboard <- rbind(leaderboard_valid, leaderboard_invalid)
  
  # add names
  index_vaild_sorted <- (1:length(mask_valid))[mask_valid][index_sorting]
  index_sorted <- c(index_vaild_sorted, (1:length(mask_valid))[-index_vaild_sorted])
  colnames(leaderboard) <- c("sharpe ratio score", "max drawdown score", "expected return score", "cpu time score", "failure ratio score", "final score")
  
  # also show original performance
  error_summary <- sapply(sapply(res$error_message, unlist), unique)[index_sorted]
  leaderboard_performance <- cbind(res$performance_summary[index_sorted, 1:3],
                                   res$cpu_time_average[index_sorted],
                                   res$failure_ratio[index_sorted])
  colnames(leaderboard_performance) <- c("sharpe ratio (median)", "max drawdown (median)", "expected return (median)", "average cpu time", "failure ratio")
  if (!is.null(res$stud_IDs)){
    stud_info <- cbind(res$stud_names[index_sorted], res$stud_IDs[index_sorted])
    rownames(leaderboard)<- rownames(leaderboard_performance) <- stud_info[, 2]
    return(list(
      "stud_info" = stud_info,
      "leaderboard_scores" = leaderboard,
      "leaderboard_performance" = leaderboard_performance,
      "error_summary" = error_summary))
  } else {
    rownames(leaderboard) <- names(res$error)
    return(list(
      "leaderboard" = leaderboard,
      "leaderboard_performance" = leaderboard_performance,
      "error_summary" = error_summary))
  }
}

rank_percentile <- function(x) {
  N <- length(x)
  rank_pctl <- ecdf(x)(x)
  rank_pctl <- (rank_pctl - 1/N)/(1 - 1/N)
  return (100*rank_pctl)
}
  
