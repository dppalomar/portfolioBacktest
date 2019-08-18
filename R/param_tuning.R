# function factory (https://adv-r.hadley.nz/function-factories.html)
fun_factory <- function(mother_fun, ...) {
  fun_params <- list(...)
  lapply(fun_params, force)
  
  new_fun <- mother_fun
  environment(new_fun) <- new.env()
  for (i in 1:length(fun_params))
    assign(names(fun_params)[i], fun_params[[i]], envir = environment(new_fun))
  #rlang::env_print(new_fun)
  #rlang::fn_env(new_fun)$lookback
  return(new_fun)
}



#' @title Generate multiple versions of a function with randomly chosen parameters
#' 
#' @description Portfolio functions usually contain some parameters that can be tuned. 
#' This function creates multiple versions of a function with randomly chosen parameters.
#' 
#' @param portfolio_fun Portfolio function with parameters unspecified.
#' @param params_grid Named list containing for each parameter the possible values it can take.
#' @param name String with the name of the portfolio function.
#' @param N_realizations Number of random realizations of the parameters.
#'                         
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @examples
#' # define GMVP with parameters "lookback" and "N_stocks"
#' GMVP_portfolio_fun <- function(data) {
#'   prices <- tail(dataset$adjusted, lookback)
#'   prices <- prices[, 1:N_stocks]
#'   X <- diff(log(prices))[-1]
#'   Sigma <- cov(X)
#'   # design GMVP
#'   w <- solve(Sigma, rep(1, nrow(Sigma)))
#'   return(w/sum(w))
#' }
#' 
#' portfolio_list <- genRandomFuns(portfolio_fun = GMVP_portfolio_fun, 
#'                                 params_grid = list(lookback = c(100, 120, 140, 160),
#'                                                     N_stocks = c(5, 10, 20, 30, 40)),
#'                                 name = "GMVP", 
#'                                 N_realizations = 10)
#' names(portfolio_list)
#' portfolio_list[[1]]
#' rlang::env_print(portfolio_list[[1]])
#' rlang::fn_env(portfolio_list[[1]])$lookback
#' rlang::fn_env(portfolio_list[[1]])$N_stocks
#' 
#' @export
genRandomFuns <- function(portfolio_fun, params_grid, name = "portfolio", N_realizations = NULL) {
  N_combinations <- prod(sapply(params_grid, length))
  if (is.null(N_realizations)) 
    stop("Number of random realizations N_realizations has to be specified")
  if (N_realizations > N_combinations) {
    warning("Too many realizations requested for only ", N_combinations, 
            " possible combinations. Using instead N_realizations = ", N_combinations, ".")
    N_realizations <- N_combinations
  } else 
    message("Generating ", N_realizations, " realizations out of a total of ", N_combinations, " possible combinations.")
  
  list_random_funs <- vector("list", N_realizations)
  list_random_params <- vector("list", N_realizations)
  for (i in 1:N_realizations) {
    # generate random parameters
    while (any(sapply(list_random_params, identical, 
                      params_realiz <- lapply(params_grid, FUN = sample, 1, replace = TRUE)))) TRUE
    list_random_params[[i]] <- params_realiz
    
    # create function
    list_random_funs[[i]] <- do.call(fun_factory, c("mother_fun" = portfolio_fun, params_realiz))
    attr(list_random_funs[[i]], "params") <- params_realiz
    names(list_random_funs)[i] <- 
      paste0(name, " (", paste(names(params_grid), params_realiz, sep = "=", collapse = ", "), ")")
  }
  attr(list_random_funs, "params_grid") <- params_grid
  return(list_random_funs)
}

plotPerformanceVsParam <- function(bt_all_portfolios, params_subset = NULL, name_performance = "Sharpe ratio", summary_fun = median) {
  # summarize performance chosen
  res_summary <- backtestSummary(bt_all_portfolios, summary_fun = summary_fun)
  score_all_funs <- summaryTable(res_summary, measures = name_performance, type = "simple")
  
  # get complete data.frame from the backtest
  N_portfolios <- length(attr(bt_all_portfolios, "portfolio_index"))
  if (N_portfolios == 0) stop("No portfolio found in backtest!")
  params_portfolio_funs_list <- lapply(bt_all_portfolios[1:N_portfolios], attr, "params")
  if (any(sapply(params_portfolio_funs_list, is.null))) stop("Backtest does not contain the attribute \"params\"!")
  params_portfolio_funs <- do.call(rbind.data.frame, params_portfolio_funs_list)
  portfolio_data <- cbind(params_portfolio_funs, score = score_all_funs[1:N_portfolios])
  
  # subset data.frame with subseting parameters
  for (i in seq_along(params_subset))
    portfolio_data <- portfolio_data[portfolio_data[[names(params_subset[i])]] %in% params_subset[[i]], ]
  
  # compute resulting params grid and indices of different types of parameters
  params_grid <- params_portfolio_funs_list[[1]]
  params_grid[] <- NA
  for (i in 1:length(params_grid))
    params_grid[[i]] <- sort(unique(sapply(params_portfolio_funs_list, function(x) x[[i]])))
  if (!is.null(params_subset)) {
    if (!all(names(params_subset) %in% names(params_grid)))
      stop("Argument \"params_subset\" contains parameters not contained in the backtest.")
    for (name in names(params_subset))
      if (!all(params_subset[[name]] %in% params_grid[[name]]))
        stop("Element ", name, " of argument \"params_subset\" is not contained in the backtest.")
    params_grid <- modifyList(params_grid, params_subset)
  }
  cat("Parameter grid:\n"); print(params_grid)
  N_grid <- sapply(params_grid, length)
  idx_fixed <- which(N_grid == 1)
  idx_numeric <- setdiff(which(lapply(params_grid, class) == "numeric"), idx_fixed)
  idx_factor <- setdiff(which(lapply(params_grid, class) %in% c("character", "factor")), idx_fixed)
  if (!setequal(union(union(idx_fixed, idx_numeric), idx_factor), 1:length(params_grid)))
    stop("Error in the partitioning of the elements of params into fixed, numeric, and factor.")
  cat(sprintf("Parameter types: %d fixed, %d variable numeric, and %d variable string/factor.", 
              length(idx_fixed), length(idx_numeric), length(idx_factor)))
  
  # plot
  title_name <- ifelse(length(idx_fixed) == 0, name_performance,
                       paste(name_performance, "for configuration:", 
                             paste(names(params_grid[idx_fixed]), params_grid[idx_fixed], sep = "=", collapse = ", ")))
  switch(as.character(length(idx_numeric)),
         "0" = stop("No numeric parameter to plot!"),
         "1" = {
           p <- ggplot(portfolio_data, aes_string(x = names(params_grid[idx_numeric]), y = "score")) +
             geom_point() + geom_line() +
             ggtitle(title_name)
           if (length(idx_factor) >= 1)  # first factor to color
             p <- p + aes_string(col = names(params_grid[idx_factor[1]]))
           if (length(idx_factor) >= 2)  # second factor to shape
             p <- p + aes_string(shape = names(params_grid)[idx_factor[2]])
           if (length(idx_factor) == 3)  # third factor to facets
             p <- p + facet_wrap(as.formula(paste("~", names(params_grid[idx_factor[3]]))), 
                                 labeller = labeller(.cols = label_both))
           if (length(idx_factor) == 4)  # third and fourth factor to facets
             p <- p + facet_wrap(as.formula(paste(names(params_grid[idx_factor[3]]), "~", names(params_grid[idx_factor[4]]))), 
                                 labeller = labeller(.cols = label_both))
           if (length(idx_factor) > 4)
             stop("Cannot deal with one numeric parameter and more than 4 string parameters.")
         },
         "2" = {
           p <- ggplot(portfolio_data, 
                       aes_string(x = names(params_grid[idx_numeric[1]]), y = names(params_grid[idx_numeric[2]]), fill = "score")) +
             geom_tile() +  # geom_raster()
             viridis::scale_fill_viridis(name = name_performance, na.value = "transparent") +
             ggtitle(title_name)
           if (length(idx_factor) == 1)  # first factor to facets
             p <- p + facet_wrap(as.formula(paste("~", names(params_grid[idx_factor[1]]))), 
                                 labeller = labeller(.cols = label_both))
           if (length(idx_factor) == 2)  # first and second factors to facets
             p <- p + facet_wrap(as.formula(paste(names(params_grid[idx_factor[1]]), "~", names(params_grid[idx_factor[2]]))), 
                                 labeller = labeller(.cols = label_both))
           if (length(idx_factor) > 2)
             stop("Cannot deal with 2 numeric parameters and more than 2 string parameters.")
         },
         stop("Cannot deal with more than 2 numeric parameters."))
  return(p)
}
  
  
  
  
plotPerformanceVsParam_old <- function(portfolio_funs, bt_summary, params_nominal = list(NULL), measure = "Sharpe ratio") {
  score_all_funs <- summaryTable(bt_summary, measures = measure, type = "simple")
  params_portfolio_funs <- lapply(portfolio_funs, attr, "params")
  params_grid <- attr(portfolio_funs, "params_grid")
  params_grid <- modifyList(params_grid, params_nominal)
  N_grid <- sapply(params_grid, length)
  switch(as.character(sum(N_grid > 1)),
         "0" = stop("No parameter to be swept!"),
         "1" = {
           i_param1 <- which.max(N_grid)  # index of parameter to sweep
           param1_sweep <- params_grid[[i_param1]]
           param1_name <- names(params_grid)[i_param1]
           score_sweep <- rep(NA, length(param1_sweep))
           message("Sweeping over one single parameter: ", param1_name, ".")
           for (i in 1:length(param1_sweep)) {
             param_grid_point <- params_grid
             param_grid_point[[i_param1]] <- param1_sweep[i]
             i_fun <- which(sapply(params_portfolio_funs, identical, param_grid_point))
             if (length(i_fun) > 1)
               stop("More than one function in the list coincides with grid point ", param_grid_point)
             if (length(i_fun) == 1)
               score_sweep[i] <- score_all_funs[i_fun]
           }
           # plot
           #plot(param1_sweep, score_sweep, type = "b", xlab = param1_name, ylab = "Sharpe ratio", 
           #     main = "Performance vs parameter")
           ggplot2::qplot(param1_sweep, score_sweep, geom = c("point", "line"), xlab = param1_name, ylab = "Sharpe ratio",
                          main = "Performance vs parameter")
         },
         "2" = {
           i_param1 <- which.max(N_grid)[1];            i_param2 <- order(N_grid, decreasing = TRUE)[2]
           param1_sweep <- params_grid[[i_param1]];     param2_sweep <- params_grid[[i_param2]]
           param1_name <- names(params_grid)[i_param1]; param2_name <- names(params_grid)[i_param2]
           score_sweep <- matrix(NA, length(param1_sweep), length(param2_sweep))
           rownames(score_sweep) <- param1_sweep
           colnames(score_sweep) <- param2_sweep
           message("Sweeping over two parameters: ", param1_name, " and ", param2_name, ".")
           for (i in 1:length(param1_sweep))
             for (j in 1:length(param2_sweep)) {
               param_grid_point <- params_grid
               param_grid_point[[i_param1]] <- param1_sweep[i]
               param_grid_point[[i_param2]] <- param2_sweep[j]
               i_fun <- which(sapply(params_portfolio_funs, identical, param_grid_point))
               if (length(i_fun) > 1)
                 stop("More than one function in the list coincides with grid point ", param_grid_point)
               if (length(i_fun) == 1)
                 score_sweep[i, j] <- score_all_funs[i_fun]
             }
           # plot
           #heatmap(score_sweep, Rowv = NA, Colv = NA, col = heat.colors(256))
           #heatmap(score_sweep, Rowv = NA, Colv = NA, col = viridisLite::inferno(256))
           melted_score <- reshape2::melt(score_sweep)
           ggplot2::ggplot(melted_score, ggplot2::aes(x = Var1, y = Var2, fill = value)) + 
             ggplot2::geom_tile() + #ggplot2::geom_raster() +
             ggplot2::labs(title = "Performance vs parameters", x = param1_name, y = param2_name) +
             viridis::scale_fill_viridis(name = measure, na.value = "transparent")
           #scale_fill_gradient(low = "white", high = "steelblue")
         },
         stop("Cannot sweep over more than 1 parameter."))
}




