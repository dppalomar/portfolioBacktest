#' @title The leaderboard of portfolio function or students
#' 
#' @description Ranking the portfolio functions according to passed criteria
#' 
#' @param res Returned result from function \code{multiplePortfolioBacktest}
#' @param weights weights for \code{sharpe ratio}, \code{max drawdown}, 
#'                \code{cpu time} and \code{failure ratio}
#'
#' @return a matrix as the leaderboard
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @export
portfolioLeaderboard <- function(res = NA, weights = c(1, 1, 1, 7)) {
  if (length(weights) != 4) stop("argument \"weights\" must have 4 elements")
  
  # sort the vaild scores
  weights <- 100 * weights / (sum(weights))
  mask_valid <- res$failure_ratio != 1
  scores <- cbind(percentile(res$performance_summary[mask_valid, 1]),
                  percentile(-res$performance_summary[mask_valid, 2]),
                  percentile(-res$cpu_time_average[mask_valid]),
                  percentile(-res$failure_ratio[mask_valid]))
  scores <- scores * matrix(weights, nrow(scores), ncol(scores), byrow = TRUE)
  performance <- scores %*% cbind(rep(1, ncol(scores)))
  
  index_sorting <- sort(performance, decreasing = TRUE, index = TRUE)$ix
  
  # combine the valid and invalid scores
  leaderboard_vaild <- cbind(scores[index_sorting, ], performance[index_sorting])
  leaderboard_invai <- matrix(NA, sum(!mask_valid), 5)
  leaderboard <- rbind(leaderboard_vaild, leaderboard_invai)
  
  # add names
  index_vaild_sorted <- (1:length(mask_valid))[mask_valid][index_sorting]
  index_sorted <- c(index_vaild_sorted, (1:length(mask_valid))[-index_vaild_sorted])
  colnames(leaderboard) <- c("sharpe ratio", "max drawdown", "cpu time", "failure ratio", "final score")
  if (!is.null(res$stud_IDs)){
    stud_info <- cbind(res$stud_names[index_sorted], res$stud_IDs[index_sorted])
    rownames(leaderboard) <- stud_info[, 2]
    return(list(
      "stud_info" = stud_info,
      "leaderboard" = leaderboard))
  } else {
    rownames(leaderboard) <- names(res$error)
    return(list("leaderboard" = leaderboard))
  }
}

percentile <- function(score) {
  ecdf_ <- ecdf(score)
  return(ecdf_(score))
}