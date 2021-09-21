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
#'                 `Sterling ratio`, `Omega ratio`, `ROT bps`, etc.
#' @param caption Table caption (only works for \code{type = "DT"}).
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
#' quintile_portfolio <- function(data, ...) {
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
summaryTable <- function(bt_summary, measures = NULL, caption = "Performance table",
                         type = c("simple", "DT", "grid.table"), 
                         order_col = NULL, order_dir = c("asc", "desc"), page_length = 10) {
  if (is.null(measures)) measures <- rownames(bt_summary$performance_summary)  # by default use all
  # extract performance measures
  real_measures <- intersect(measures, rownames(bt_summary$performance_summary))
  performance <- bt_summary$performance_summary[real_measures, , drop = FALSE]
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
                              caption = caption)
           p <- DT::formatStyle(p, 0, target = "row", fontWeight = DT::styleEqual(c("uniform", "index"), c("bold", "bold")))
           p <- DT::formatRound(p, colnames(performance), digits = 2)
           if ("annual volatility" %in% colnames(performance))
             p <- DT::formatPercentage(p, "annual volatility", 1)
           if ("max drawdown" %in% colnames(performance))
             p <- DT::formatPercentage(p, "max drawdown", 1)
           if ("ROT (bps)" %in% colnames(performance))
             p <- DT::formatRound(p, "ROT (bps)", digits = 0)
           if ("cpu time" %in% colnames(performance))
             p <- DT::formatRound(p, "cpu time", digits = 4)
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
#' @seealso \code{\link{summaryTable}}, \code{\link{backtestBoxPlot}},
#'          \code{\link{backtestChartCumReturn}}, \code{\link{backtestChartDrawdown}},
#'          \code{\link{backtestChartStackedBar}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data, ...) {
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
#' @importFrom ggplot2 ggplot aes aes_string geom_bar scale_x_discrete facet_wrap labs
#' @importFrom rlang .data
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
           ggplot(df, aes(x = .data$Var1, y = .data$Freq, fill = .data$Var1)) + 
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
#' @seealso \code{\link{summaryBarPlot}}, \code{\link{backtestChartCumReturn}}, 
#'          \code{\link{backtestChartDrawdown}}, \code{\link{backtestChartStackedBar}}
#'          
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data, ...) {
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
#' # now we can plot
#' backtestBoxPlot(bt, "Sharpe ratio")
#' backtestBoxPlot(bt, "Sharpe ratio", type = "simple")
#' }
#' 
#' @importFrom grDevices topo.colors
#' @importFrom graphics boxplot par
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes aes_string geom_boxplot geom_point scale_x_discrete coord_flip labs
#' @importFrom rlang .data
#' @export
backtestBoxPlot <- function(bt, measure = "Sharpe ratio", type = c("ggplot2", "simple"), ...) {
  # extract correct performance measure
  res_list_table <- backtestTable(bt)
  # idx <- grep(measure, names(res_list_table), ignore.case = TRUE)  # it does not work when "measure" contains brackets
  idx <- which(measure == names(res_list_table))
  if (length(idx)!=1) stop(measure, " does not match a single performance measure")
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
           ggplot(df, aes(x = .data$Var2, y = .data$Freq, fill = .data$Var2)) +
             geom_boxplot(show.legend = FALSE) +  # (outlier.shape = NA)
             geom_point(size = 0.5, alpha = params$alpha, show.legend = FALSE) +  # geom_jitter(width = 0) +
             scale_x_discrete(limits = rev(levels(df$Var2))) +
             coord_flip(ylim = plot_limits) + 
             labs(title = measure, x = NULL, y = NULL)
         },
         stop("Boxplot type unknown."))
}




#' @title Chart of the cumulative returns or wealth for a single backtest
#' 
#' @description Create chart of the cumulative returns or wealth for a single backtest
#' obtained with the function \code{\link{portfolioBacktest}}.
#' By default the chart is based on the package \code{ggplot2}, but the user can also 
#' specify a plot based on \code{PerformanceAnalytics}.
#' 
#' @inheritParams backtestBoxPlot
#' @param portfolios String with portfolio names to be charted. 
#'                   Default charts all portfolios in the backtest.
#' @param dataset_num Dataset index to be charted. Default is \code{dataset_num = 1}.
#' 
#' @param ... Additional parameters.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{summaryBarPlot}}, \code{\link{backtestBoxPlot}}, 
#'          \code{\link{backtestChartDrawdown}}, \code{\link{backtestChartStackedBar}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data, ...) {
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
#' # now we can chart
#' backtestChartCumReturn(bt)
#' }
#' 
#' @importFrom grDevices topo.colors
#' @importFrom graphics par
#' @importFrom PerformanceAnalytics chart.CumReturns
#' @importFrom ggplot2 ggplot fortify aes geom_line theme element_blank ggtitle xlab ylab
#' @importFrom rlang .data
#' @export
backtestChartCumReturn <- function(bt, portfolios = names(bt), dataset_num = 1, type = c("ggplot2", "simple"), ...) {
  # extract data
  bt <- bt[portfolios]
  wealth <- do.call(cbind, lapply(bt, function(x) x[[dataset_num]]$wealth))
  return <- do.call(cbind, lapply(bt, function(x) x[[dataset_num]]$return))
  colnames(return) <- colnames(wealth) <- names(bt)

  # plot
  params <- list(...)
  switch(match.arg(type),
         "simple" = {
           if (is.null(params$col)) params$col <- topo.colors(length(bt))
           chart.CumReturns(return, main = "Cumulative Return", wealth.index = TRUE, legend.loc = "topleft", colorset = params$col)
         },
         "ggplot2" = {
           ggplot(fortify(wealth, melt = TRUE), aes(x = .data$Index, y = .data$Value, col = .data$Series)) +
             geom_line() +
             theme(legend.title = element_blank()) +
             #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", date_minor_breaks = "1 week")
             ggtitle("Cumulative Return") + xlab(element_blank()) + ylab(element_blank())
         },
         stop("Unknown plot type."))
}




#' @title Chart of the drawdown for a single backtest
#' 
#' @description Create chart of the drawdown for a single backtest
#' obtained with the function \code{\link{portfolioBacktest}}.
#' By default the chart is based on the package \code{ggplot2}, but the user can also 
#' specify a plot based on \code{PerformanceAnalytics}.
#' 
#' @inheritParams backtestChartCumReturn
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{summaryBarPlot}}, \code{\link{backtestBoxPlot}}, 
#'          \code{\link{backtestChartCumReturn}}, \code{\link{backtestChartStackedBar}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # define your own portfolio function
#' quintile_portfolio <- function(data, ...) {
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
#' # now we can chart
#' backtestChartDrawdown(bt)
#' }
#' 
#' @importFrom grDevices topo.colors
#' @importFrom graphics par
#' @importFrom PerformanceAnalytics Drawdowns chart.Drawdown
#' @importFrom ggplot2 ggplot fortify aes geom_line theme element_blank ggtitle xlab ylab
#' @importFrom rlang .data
#' @export
backtestChartDrawdown <- function(bt, portfolios = names(bt), dataset_num = 1, type = c("ggplot2", "simple"), ...) {
  # extract data
  bt <- bt[portfolios]
  return <- do.call(cbind, lapply(bt, function(x) x[[dataset_num]]$return))
  colnames(return) <- names(bt)
  drawdown <- Drawdowns(return)
  
  # plot
  params <- list(...)
  switch(match.arg(type),
         "simple" = {
           if (is.null(params$col)) params$col <- topo.colors(length(bt))
           chart.Drawdown(return, main = "Drawdown", legend.loc = "bottomleft", colorset = params$col)
         },
         "ggplot2" = {
           ggplot(fortify(drawdown, melt = TRUE), aes(x = .data$Index, y = .data$Value, col = .data$Series)) +
             geom_line() +
             theme(legend.title = element_blank()) +
             ggtitle("Drawdown") + xlab(element_blank()) + ylab(element_blank())
         },
         stop("Unknown plot type."))
}




#' @title Chart of the weight allocation over time for a portfolio over a single backtest
#' 
#' @description Create chart of the weight allocation over time for a portfolio over a single 
#' backtest obtained with the function \code{\link{portfolioBacktest}}.
#' By default the chart is based on the package \code{ggplot2}, but the user can also 
#' specify a plot based on \code{PerformanceAnalytics}.
#' 
#' @inheritParams backtestChartCumReturn
#' @param portfolio String with portfolio name to be charted. 
#'                  Default charts the first portfolio in the backtest.
#' @param legend Boolean to choose whether legend is plotted or not. Default is \code{legend = FALSE}.
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @seealso \code{\link{summaryBarPlot}}, \code{\link{backtestBoxPlot}}, 
#'          \code{\link{backtestChartCumReturn}}, \code{\link{backtestChartDrawdown}}
#' 
#' @examples
#' \donttest{
#' library(portfolioBacktest)
#' data(dataset10)  # load dataset
#' 
#' # for better illustration, let's use only the first 5 stocks
#' dataset10_5stocks <- lapply(dataset10, function(x) {x$adjusted <- x$adjusted[, 1:5]; return(x)})
#' 
#' # define GMVP (with heuristic not to allow shorting)
#' GMVP_portfolio_fun <- function(dataset, ...) {
#'   X <- diff(log(dataset$adjusted))[-1]  # compute log returns
#'   Sigma <- cov(X)  # compute SCM
#'   # design GMVP
#'   w <- solve(Sigma, rep(1, nrow(Sigma)))
#'   w <- abs(w)/sum(abs(w))
#'   return(w)
#' }
#' 
#' # backtest
#' bt <- portfolioBacktest(list("GMVP" = GMVP_portfolio_fun), dataset10_5stocks, rebalance_every = 20)
#' 
#' # now we can chart
#' backtestChartStackedBar(bt, "GMVP", type = "simple")
#' backtestChartStackedBar(bt, "GMVP", type = "simple", legend = TRUE)
#' backtestChartStackedBar(bt, "GMVP")
#' backtestChartStackedBar(bt, "GMVP", legend = TRUE)
#' }
#' 
#' @importFrom PerformanceAnalytics chart.StackedBar
#' @importFrom ggplot2 ggplot fortify aes geom_bar ggtitle xlab element_blank ylab labs theme
#' @importFrom rlang .data
#' 
#' @export
backtestChartStackedBar <- function(bt, portfolio = names(bt[1]), dataset_num = 1, type = c("ggplot2", "simple"), legend = FALSE) {
  # extract data
  w <- bt[[portfolio]][[dataset_num]]$w_designed
  title <- sprintf("Weight allocation over time for %s", portfolio)
  
  # plot
  #params <- list(...)
  switch(match.arg(type),
         "simple" = {
           #if (is.null(params$col)) params$col <- topo.colors(nrow(w))
           legend.loc <- if (legend) "under" else NULL
           chart.StackedBar(w, main = title,
                            ylab = "weights", space = 0, border = NA, legend.loc = legend.loc)
         },
         "ggplot2" = {
           p <- ggplot(fortify(w, melt = TRUE), aes(x = .data$Index, y = .data$Value, fill = .data$Series)) +
             geom_bar(stat = "identity", color = "black") +
             ggtitle(title) + xlab(element_blank()) + ylab("weight")
           if (legend)
             p <- p + labs(fill = "Assets") #theme(legend.title = element_blank())
           else
             p <- p + theme(legend.position = "none")
           p
         },
         stop("Unknown plot type."))
}

