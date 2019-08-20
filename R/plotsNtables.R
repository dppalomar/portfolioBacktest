#' @title Create table from backtest summary
#' 
#' @description After performing a backtest with \code{\link{portfolioBacktest}} 
#' and obtaining a summary of the performance measures with 
#' \code{\link{backtestSummary}}, this function creates a table from the summary. 
#' By default the table is a simple matrix, but if the user has installed the 
#' package \code{DT} or \code{grid.table} nicer tables can be generated.
#' 
#' @param bt_summary Backtest summary as obtained from the function \code{backtestSummary}.
#' @param measures String vector to select performane measures (default is all) from
#'                 `Sharpe ratio`, `max drawdown`, `annual return`, `annual volatility`, 
#'                 `Sterling ratio`, `Omega ratio`, and `ROT bps`.
#'                 
#' @param type Type of table. Valid options: \code{"simple", "DT", "grid.table"}. Default is 
#'             \code{"simple"} and generates a simple matrix (with the other choices the 
#'             corresponding package must be installed).
#' @param order_col Column number or column name of the performance measure to be used to 
#'                  sort the rows (only used for table \code{type = "DT"}). By default the 
#'                  last column will be used.
#' @param order_dir Direction to be used to sort the rows (only used for table 
#'                  \code{type = "DT"}). Valid options: \code{"asc", "desc"}. 
#'                  Default is \code{"asc"}.
#' @param page_length Page length for the table (only used for table \code{type = "DT"}). 
#'                    Default is \code{10}.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{summaryBarPlot}}
#' 
#' @examples
#' \donttest{ 
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data) {
#'   X <- diff(log(data$adjusted))[-1]  
#'   N <- ncol(X)
#'   ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
#'   w <- rep(0, N)
#'   w[ranking[1:round(N/5)]] <- 1/round(N/5)
#'   return(w)
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), 
#'                         dataset10,
#'                         benchmark = c("uniform", "index"))
#' 
#' # now we can obtain the table
#' bt_summary_median <- backtestSummary(bt)
#' summaryTable(bt_summary_median, measures = c("max drawdown", "annual volatility"))
#' summaryTable(bt_summary_median, measures = c("max drawdown", "annual volatility"), type = "DT")
#' }
#' 
#' @export
summaryTable <- function(bt_summary, measures = NULL, type = c("simple", "DT", "grid.table"), 
                         order_col = NULL, order_dir = c("asc", "desc"), page_length = 10) {
  if (is.null(measures)) measures <- c("cpu time", rownames(bt_summary$performance_summary))  # by default use all
  # extract performance measures
  real_measures <- intersect(measures, rownames(bt_summary$performance_summary))
  performance <- bt_summary$performance_summary[real_measures, , drop = FALSE]
  if ("cpu time" %in% measures)
    performance <- rbind("cpu time" = bt_summary$cpu_time_summary, performance)
  performance <- t(round(performance, 4))
  
  # show table
  switch(match.arg(type),
         "simple" = performance,
         "DT" = {
           if (!requireNamespace("DT", quietly = TRUE)) 
             stop("Please install package \"DT\" or choose another table type", call. = FALSE)
           if (is.character(order_col)) order_col <- which(colnames(performance) == order_col)
           if (is.null(order_col) || length(order_col) == 0) order_col <- ncol(performance)
           order_dir <- match.arg(order_dir)
           p <- DT::datatable(performance, 
                              options = list(pageLength = page_length, scrollX = TRUE, order = list(order_col, order_dir)),
                              caption = "Leaderboard:")
           p <- DT::formatStyle(p, 0, target = "row", fontWeight = DT::styleEqual(c("uniform", "index"), c("bold", "bold")))
           if ("annual volatility" %in% colnames(performance))
             p <- DT::formatPercentage(p, "annual volatility", 1)
           if ("max drawdown" %in% colnames(performance))
             p <- DT::formatPercentage(p, "max drawdown", 1)
           p
         },
         "grid.table" = {
           if (!requireNamespace("gridExtra", quietly = TRUE)) 
             stop("Please install package \"gridExtra\" or choose another table type", call. = FALSE)
           gridExtra::grid.table(performance)
           },
         stop("Table type unknown."))
}



#' @title Create barplot from backtest summary
#' 
#' @description After performing a backtest with \code{\link{portfolioBacktest}} 
#' and obtaining a summary of the performance measures with 
#' \code{\link{backtestSummary}}, this function creates a barplot from the summary. 
#' By default the plot is based on the package \code{ggplot2}, but the user
#' can also specify a simple base plot.
#' 
#' @inheritParams summaryTable
#' @param type Type of plot. Valid options: \code{"ggplot2", "simple"}. Default is 
#'             \code{"ggplot2"}.
#' @param ... Additional parameters (only used for plot \code{type = "simple"}); 
#'            for example: \code{mar} for margins as in \code{par()},
#'                         \code{inset} for the legend inset as in \code{legend()},
#'                         \code{legend_loc} for the legend location as in \code{legend()}.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{summaryTable}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data) {
#'   X <- diff(log(data$adjusted))[-1]  
#'   N <- ncol(X)
#'   ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
#'   w <- rep(0, N)
#'   w[ranking[1:round(N/5)]] <- 1/round(N/5)
#'   return(w)
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), dataset10,
#'                         benchmark = c("uniform", "index"))
#'                         
#' # now we can obtain the table
#' bt_summary_median <- backtestSummary(bt)
#' summaryBarPlot(bt_summary_median, measures = c("max drawdown", "annual volatility"))
#' summaryBarPlot(bt_summary_median, measures = c("max drawdown", "annual volatility"), 
#'                type = "simple")
#' }
#' 
#' @importFrom grDevices topo.colors
#' @importFrom graphics barplot legend par
#' @import ggplot2
#' @export
summaryBarPlot <- function(bt_summary, measures = NULL, type = c("ggplot2", "simple"), ...) {
  # extract table
  res_table <- summaryTable(bt_summary, measures)
  
  # plot
  params <- list(res_table, ...)
  if (is.null(params$main)) params$main <- "Performance of portfolios"
  switch(match.arg(type),
         "simple" = {
           if (is.null(params$cex.names)) params$cex.names <- 0.9
           if (is.null(params$cex.axis)) params$cex.axis <- 0.8
           if (is.null(params$col)) params$col <- topo.colors(nrow(res_table))
           if (is.null(params$beside)) params$beside <- TRUE
           mar <- if (is.null(params$mar)) c(3, 3, 3, 11)
                  else params$mar
           inset <- if (is.null(params$inset)) c(0, 0)
                     else params$inset
           legend_loc <- if (is.null(params$legend_loc)) "topleft" 
                         else params$legend_loc
           old_par <- par(mar = mar, xpd = TRUE)
           do.call(barplot, params)
           legend(legend_loc, rownames(res_table), cex = 0.8, fill = params$col, inset = inset)
           par(old_par)
         },
         "ggplot2" = {
           df <- as.data.frame.table(res_table)
           ggplot(df, aes_string(x = "Var1", y = "Freq", fill = "Var1")) + 
             geom_bar(stat = "identity") +  #position = position_dodge()
             scale_x_discrete(breaks = NULL) +
             facet_wrap(~ Var2, scales = "free_y") +
             labs(title = params$main, x = NULL, y = NULL, fill = NULL)
         },
         stop("Barplot type unknown."))
}



#' @title Create boxplot from backtest results
#' 
#' @description Create boxplot from a portfolio backtest obtained with the function 
#' \code{\link{portfolioBacktest}}. By default the boxplot is based on the 
#' package \code{ggplot2} (also plots a dot for each single backtest), but the user can also 
#' specify a simple base plot.
#' 
#' @inheritParams backtestSummary
#' @param measure String to select a performane measure from
#'                 \code{"Sharpe ratio"}, \code{"max drawdown"}, \code{"annual return"}, \code{"annual volatility"}, 
#'                 \code{"Sterling ratio"}, \code{"Omega ratio"}, and \code{"ROT bps"}.
#'                  Default is \code{"Sharpe ratio"}.
#' @param type Type of plot. Valid options: \code{"ggplot2", "simple"}. Default is 
#'             \code{"ggplot2"}.
#' @param ... Additional parameters. For example: 
#'            \code{mar} for margins as in \code{par()} (for the case of plot \code{type = "simple"}); and
#'            \code{alpha} for the alpha of each backtest dot (for the case of plot \code{type = "ggplot2"}), 
#'                         set to \code{0} to remove the dots.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data) {
#'   X <- diff(log(data$adjusted))[-1]  
#'   N <- ncol(X)
#'   ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
#'   w <- rep(0, N)
#'   w[ranking[1:round(N/5)]] <- 1/round(N/5)
#'   return(w)
#' }
#' 
#' # do backtest
#' bt <- portfolioBacktest(list("Quintile" = quintile_portfolio), dataset10,
#'                         benchmark = c("uniform", "index"))
#' 
#' # Now we can plot
#' backtestBoxPlot(bt, "Sharpe ratio")
#' backtestBoxPlot(bt, "Sharpe ratio", type = "simple")
#' }
#' 
#' @importFrom grDevices topo.colors
#' @importFrom graphics boxplot par
#' @importFrom stats quantile
#' @import ggplot2
#' @export
backtestBoxPlot <- function(bt, measure = "Sharpe ratio", type = c("ggplot2", "simple"), ...) {
  # extract correct performance measure
  res_list_table <- backtestTable(bt)
  idx <- grep(measure, names(res_list_table), ignore.case = TRUE)
  if (length(idx)!=1) stop(measure, "does not match a single performance measure")
  res_table <- res_list_table[[idx]]
  
  # plot boxplot
  params <- list(res_table[, ncol(res_table):1], ...)
  switch(match.arg(type),
         "simple" = {
           if (is.null(params$main)) params$main <- measure
           if (is.null(params$las)) params$las <- 1
           if (is.null(params$cex.axis)) params$cex.axis <- 0.8
           if (is.null(params$horizontal)) params$horizontal <- TRUE
           if (is.null(params$outline)) params$outline <- FALSE
           if (is.null(params$col)) params$col <- topo.colors(ncol(res_table))
           mar <- if (is.null(params$mar)) c(3, 10, 3, 1)
                  else params$mar
           old_par <- par(mar = mar)
           do.call(boxplot, params)  # boxplot(res_table[, ncol(res_table):1], main = measure, las = 1, cex.axis = 0.8, horizontal = TRUE, outline = FALSE, col = viridisLite::viridis(ncol(res_table)))
           par(old_par)
         },
         "ggplot2" = {
           if (is.null(params$alpha)) params$alpha <- 0.4  # this is for the points (set to 0 if not want them)
           limits <- apply(res_table, 2, function(x) {
             lquartile <- quantile(x, 0.25, na.rm = TRUE)
             uquartile <- quantile(x, 0.75, na.rm = TRUE)
             IQR <- uquartile - lquartile
             c(limit_min = min(x[x > lquartile - 1.6*IQR], na.rm = TRUE), limit_max = max(x[x < uquartile + 1.6*IQR], na.rm = TRUE))
           })
           plot_limits <- c(min(limits["limit_min", ]), max(limits["limit_max", ]))
           df <- as.data.frame.table(res_table)
           ggplot(df, aes_string(x = "Var2", y = "Freq", fill = "Var2")) +
             geom_boxplot(show.legend = FALSE) +  # (outlier.shape = NA)
             geom_point(size = 0.5, alpha = params$alpha, show.legend = FALSE) +  # geom_jitter(width = 0) +
             scale_x_discrete(limits = rev(levels(df$Var2))) +
             coord_flip(ylim = plot_limits) + 
             labs(title = measure, x = NULL, y = NULL)
         },
         stop("Boxplot type unknown."))
}

