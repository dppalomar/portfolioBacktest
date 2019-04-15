#' @title Shows a table of a backtest summary
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples 
#' # After running a backtest and extracting a performance summary, one would get:
#' bt <- portfolioBacktest(...)
#' res_summary_median <- backtestSummary(bt, summary_fun = median)
#' 
#' # Here let's generate fake random data for simplicity:
#' mat <- matrix(runif(4*6), 4, 6)
#' rownames(mat) <- c("Sharpe ratio", "max drawdown", "annual return", "annual volatility")
#' colnames(mat) <- c("portfolio 1", "portfolio 2", "portfolio 3", "portfolio 4", "portfolio 5", "portfolio 6")
#' res_summary_median <- list(performance_summary = mat)
#' 
#' # Now we can obtain the table
#' summaryTable(res_summary_median, measures = c("max drawdown", "annual volatility"))
#' summaryTable(res_summary_median, measures = c("max drawdown", "annual volatility"), type = "DT")
#' 
#' @export
summaryTable <- function(res_summary, measures = NULL, type = c("simple", "DT", "grid.table"), order_col = NULL, order_dir = "asc") {  #could be "desc"
  if (is.null(measures)) measures <- rownames(res_summary_median$performance_summary)  # by default use all
  # extract performance measures
  real_measures <- intersect(measures, rownames(res_summary_median$performance_summary))
  performance <- res_summary$performance_summary[real_measures, , drop = FALSE]
  if ("cpu time" %in% measures)
    performance <- rbind("cpu time" = res_summary$cpu_time_average, performance)
  performance <- t(round(performance, 4))
  
  # show table
  switch(match.arg(type),
         "simple" = performance,
         "DT" = {
           if (is.null(order_col)) order_col <- ncol(performance)
           p <- DT::datatable(performance, options = list(dom = 't', pageLength = 15, scrollX = TRUE, order = list(order_col, order_dir)))
           p <- DT::formatStyle(p, 0, target = "row", fontWeight = DT::styleEqual(c("uniform", "index"), c("bold", "bold")))
           if ("annual volatility" %in% colnames(performance))
             p <- DT::formatPercentage(p, "annual volatility", 1)
           if ("max drawdown" %in% colnames(performance))
             p <- DT::formatPercentage(p, "max drawdown", 1)
           p
         },
         "grid.table" = gridExtra::grid.table(performance),
         stop("Table type unknown"))
}


#' @title Plots a barplot of a backtest summary
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples 
#' # After running a backtest and extracting a performance summary, one would get:
#' bt <- portfolioBacktest(...)
#' res_summary_median <- backtestSummary(bt, summary_fun = median)
#' 
#' # Here let's generate fake random data for simplicity:
#' mat <- matrix(runif(4*6), 4, 6)
#' rownames(mat) <- c("Sharpe ratio", "max drawdown", "annual return", "annual volatility")
#' colnames(mat) <- c("portfolio 1", "portfolio 2", "portfolio 3", "portfolio 4", "portfolio 5", "portfolio 6")
#' res_summary_median <- list(performance_summary = mat)
#' 
#' # Now we can obtain the table
#' summaryBarPlot(res_summary_median, measures = c("max drawdown", "annual volatility"))
#' summaryBarPlot(res_summary_median, measures = c("max drawdown", "annual volatility"), type = "ggplot2")
#' 
#' @import ggplot2
#' @export
summaryBarPlot <- function(res_summary, measures = NULL, type = c("ggplot2", "simple"), 
                           mar = c(3, 3, 3, 11), inset = c(-0.45, 0), legend_loc = "right", ...) {
  # extract table
  res_table <- summaryTable(res_summary, measures)
  
  # plot
  switch(match.arg(type),
         "simple" = {
           params <- list(res_table, ...)
           if (is.null(params$main)) params$main <- "Performance of portfolios"
           if (is.null(params$cex.names)) params$cex.names <- 0.9
           if (is.null(params$cex.axis)) params$cex.axis <- 0.8
           if (is.null(params$col)) params$col <- viridisLite::viridis(nrow(res_table))  # topo.colors(nrow(res_table))
           if (is.null(params$beside)) params$beside <- TRUE
           old_par <- par(mar = mar, xpd=TRUE)
           do.call(barplot, params)
           legend(legend_loc, rownames(res_table), cex = 0.8, fill = params$col, inset = inset)
           par(old_par)
         },
         "ggplot2" = {
           df <- as.data.frame.table(res_table)
           ggplot(df, aes(x = Var1, y = Freq, fill = Var1)) + 
             geom_bar(stat = "identity") +
             scale_x_discrete(breaks = NULL) +
             facet_wrap(~ Var2, scales = "free_y") +
             labs(title = "Performance of portfolios", x = NULL, y = NULL, fill = NULL)
             #scale_fill_manual(values = viridisLite::viridis(nrow(res_table)))
             #viridis::scale_fill_viridis(discrete = TRUE)  #this requires viridis instead of viridisLite
           # ggplot(df, aes(x = Var2, y = Freq, fill = Var1)) + 
           #   geom_bar(stat = "identity", color = "black", position = position_dodge())
         },
         stop("Table type unknown"))
}



#' @title Plots a boxplot from a backtest
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples 
#' # After running a backtest and extracting a performance summary, one would get:
#' bt <- portfolioBacktest(...)
#' 
#' # Here let's generate fake random data for simplicity  
#' TODO (Rui): needs to be fixed
#' 
#' fun1_1 <- list()
#' fun1_1$performance <- runif(1)
#' names(fun1_1$performance) <- "Sharpe ratio"
#' fun1_2 <- list()
#' fun1_2$performance <- runif(1)
#' names(fun1_2$performance) <- "Sharpe ratio"
#' fun1 <- list(fun1_1, fun1_1)
#' fun2 <- list(fun1_1, fun1_1)
#' bt <- list("fun1" = fun1, "fun2" = fun2)
#' 
#' # Now we can plot
#' backtestBoxPlot(bt, "Sharpe ratio")
#' 
#' @import ggplot2
#' @export
backtestBoxPlot <- function(backtest, measure = "Annual volatility", type = c("ggplot2", "simple"), mar = c(3, 10, 3, 1), ...) {
  # extract correct performance measure
  res_list_table <- backtestTable(backtest)
  idx <- grep(measure, names(res_list_table), ignore.case = TRUE)
  if (length(idx)!=1) stop(measure, "does not match a single performance measure")
  res_table <- res_list_table[[idx]]
  
  # plot boxplot
  switch(match.arg(type),
         "simple" = {
           params <- list(res_table[, ncol(res_table):1], ...)
           if (is.null(params$main)) params$main <- measure
           if (is.null(params$las)) params$las <- 1
           if (is.null(params$cex.axis)) params$cex.axis <- 0.8
           if (is.null(params$horizontal)) params$horizontal <- TRUE
           if (is.null(params$outline)) params$outline <- FALSE
           if (is.null(params$col)) params$col <- viridisLite::viridis(ncol(res_table))  # topo.colors(ncol(res_table))
           old_par <- par(mar = mar)
           do.call(boxplot, params)  # boxplot(res_table[, ncol(res_table):1], main = measure, las = 1, cex.axis = 0.8, horizontal = TRUE, outline = FALSE, col = viridisLite::viridis(ncol(res_table)))
           par(old_par)
         },
         "ggplot2" = {
           # res_table_clean <- apply(res_table, 2, function(x) {  # remove outliers for better plotting?
           #   lquartile <- quantile(x, 0.25)
           #   uquartile <- quantile(x, 0.75)
           #   IQR <- uquartile - lquartile
           #   x_without_outliers <- ifelse(x > lquartile - 1.5*IQR & x < uquartile + 1.5*IQR, x, NA)
           #   return(x_without_outliers)
           # })
           limits <- apply(res_table, 2, function(x) {
             lquartile <- quantile(x, 0.25, na.rm = TRUE)
             uquartile <- quantile(x, 0.75, na.rm = TRUE)
             IQR <- uquartile - lquartile
             c(limit_min = min(x[x > lquartile - 1.6*IQR], na.rm = TRUE), limit_max = max(x[x < uquartile + 1.6*IQR], na.rm = TRUE))
           })
           plot_limits <- c(min(limits["limit_min", ]), max(limits["limit_max", ]))
           df <- as.data.frame.table(res_table)
           ggplot(df, aes(x = Var2, y = Freq, fill = Var2)) +
             geom_boxplot(show.legend = FALSE) +  # (outlier.shape = NA)
             geom_point(size = 0.5, show.legend = FALSE) +  # geom_jitter(width = 0) +
             scale_x_discrete(limits = rev(levels(df$Var2))) +
             coord_flip(ylim = plot_limits) + 
             labs(title = measure, x = NULL, y = NULL)
             #theme_bw() + #theme(legend.position = "none") + 
             #scale_fill_manual(values = viridis(ncol(res_table)))
         },
         stop("Boxplot type unknown"))
}

