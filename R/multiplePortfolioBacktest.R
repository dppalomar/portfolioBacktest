#' @title Evaluating multiple portfolio functions defined by customer
#'
#' @description Evaluate multiple portfolio functions written in format form
#'
#' @param folder_path path for a folder which contains all (and only) functions to be evaluated
#' @param portfolio_fun_list a list of portfolio functions, valid when \code{folder_path} is not passed
#' @param prices a list of \code{xts} containing the stock prices for the backtesting.
#' @param return_all logical, indicating whether return all the results.
#' @param return_portfolio logical value, whether return portfolios.
#' @param shortselling whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage).
#' @param T_rolling_window length of the rolling window.
#' @param optimize_every how often the portfolio is to be optimized.
#' @param rebalance_every how often the portfolio is to be rebalanced.
#' @return A list containing the performance in the following elements:
#' \item{\code{stud_names}  }{string vector, recording the student names extracted from files' name.}
#' \item{\code{stud_IDs}  }{string vector, recording the student IDs extracted from files' name.}
#' \item{\code{performance_summary}}{matrix, indicating each student's performance summary in each row}
#' \item{\code{time_average}}{vector, recording the average execution time for successful application of portfolio functions}
#' \item{\code{failure_ratio}}{vector, recording the failure ratio of applying given portfolio function to different data examples}
#' \item{\code{error_message}}{string list, recording the error message when error happens}
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @import xts
#'         PerformanceAnalytics
#' @export
multiplePortfolioBacktest <- function(folder_path = NULL, portfolio_fun_list = NULL, prices, return_all = FALSE, ...) {
  
  if (is.null(folder_path) && is.null(portfolio_fun_list)) stop("The \"folder_path\" and \"portfolio_fun_list\" can not both be NULL")
  # when pass a list of function
  if (!is.null(portfolio_fun_list)) 
    return(multiplePortfoioBacktestPassFunctions(portfolio_function_list, prices, return_all, ...))
  
  # extract useful informations and init all
  files <- list.files(folder_path)
  stud_names <- stud_IDs <- c()
  time_average <- rep(NA, length(files))
  failure_ratio <- rep(1, length(files))
  error_message <- list()
  portfolios_perform <- matrix(NA, length(files), 4)
  if (return_all) results_container <- list()
  
  # save the package and variables list
  packages_default <- search()
  var_fun_default <- ls()
  
  # some functions evaluation here
  for (i in 1:length(files)) {
    
    file <- files[i]
    file_name_cleaned <- gsub("\\s+", "", file)
    tmp <- unlist(strsplit(file_name_cleaned, "-|\\."))
    stud_names <- c(stud_names, paste(tmp[1], tmp[2], collapse = " "))
    stud_IDs <- c(stud_IDs, tmp[3])
    cat(paste0(Sys.time()," - Execute code from ", stud_names[i], " (", stud_IDs[i], ")\n"))
    
    # mirror list of present variables and functions
    var_fun_default <- ls()
    
    
    tryCatch({
      suppressMessages(source(paste0(folder_path, "/", file), local = TRUE))
      res <- portfolioBacktest(portfolio_fun = portfolio_fun, prices = prices, ...)
      portfolios_perform[i, ] <- res$performance_summary
      time_average[i] <- res$cpu_time_average
      failure_ratio[i] <- res$failure_ratio
      error_message[[i]] <- res$error_message
      if (return_all) results_container[[i]] <- res
    }, warning = function(w){
      error_message[[i]] <<- w$message
    }, error = function(e){
      error_message[[i]] <<- e$message
    })
    
    
    # detach the newly loaded packages
    packages_now <- search()
    packages_det <- packages_now[!(packages_now %in% packages_default)]
    detach_packages(packages_det)
    
    # delete students' function
    var_fun_now <- ls()
    var_fun_det <- var_fun_now[!(var_fun_now %in% var_fun_default)]
    rm(list = var_fun_det)
  }
  
  rownames(portfolios_perform) <- names(time_average) <- names(failure_ratio) <- names(error_message) <- stud_IDs
  colnames(portfolios_perform) <- paste(c("sharpe ratio", "max drawdown", "expected return", "volatility"), " (median)")

  vars_tb_returned <- list("stud_names" = stud_names,
                           "stud_IDs" = stud_IDs,
                           "performance_summary" = portfolios_perform,
                           "cpu_time_average" = time_average,
                           "failure_ratio" = failure_ratio,
                           "error_message" = error_message)
  if (return_all) {
    names(results_container) <- stud_IDs
    vars_tb_returned$results_container <- results_container
  }
  return(vars_tb_returned)
}

multiplePortfoioBacktestPassFunctions <- function(portfolio_function_list, prices, return_all, ...) {
  
  if (!is.list(portfolio_function_list)) stop("argument \"portfolio_function_list\" must be a list")
  
  n_function <- length(portfolio_function_list) 
  if (is.null(names(portfolio_function_list))) func_names <- paste0("func", 1:n_function)
  else func_names <- names(portfolio_function_list)
  
  cpu_time_average <- rep(NA, n_function)
  failure_ratio <- rep(1, n_function)
  error_message <- list()
  performance_summary <- matrix(NA, n_function, 4)
  if (return_all) results_container <- list()
  
  names(cpu_time_average) <- names(failure_ratio) <-  rownames(performance_summary) <- func_names
  colnames(performance_summary) <- paste(c("sharpe ratio", "max drawdown", "expected return", "volatility"), " (median)")
  
  for (i in 1:n_function) {
    # report status
    cat(paste0(Sys.time()," - Execute ", func_names[i], "\n"))
    res <- portfolioBacktest(portfolio_fun = portfolio_function_list[[i]], prices = prices, ...)
    performance_summary[i, ] <- res$performance_summary
    cpu_time_average[i] <- res$cpu_time_average
    failure_ratio[i] <- res$failure_ratio
    error_message[[i]] <- res$error_message
    if (return_all) results_container[[i]] <- res
  }
  
  names(error_message) <- func_names
  vars_tb_returned <- list("performance_summary" = performance_summary,
                           "cpu_time_average" = cpu_time_average,
                           "failure_ratio" = failure_ratio,
                           "error_message" = error_message)
  if (return_all) {
    names(results_container) <- func_names
    vars_tb_returned$results_container <- results_container
  }
  return(vars_tb_returned)
}

detach_packages <- function(items) {
  for (item in items) {
    if (item %in% search()) {
      detach(item, unload = TRUE, character.only = TRUE)
    }
  }
}


#' @title Checking uninstalled packages written in the portfolio functions defined by customer
#'
#' @description Checke uninstalled packages of portfolio functions written in format form
#'
#' @param folder_path Path for a folder which contains all (and only) functions to be evaluated
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
checkUninstalledPackages <- function(folder_path) {
  if (!require("readtext")) stop("Package \"readtext\" is required to run this function!")
  if (!require("stringi")) stop("Package \"stringi\" is required to run this function!")
  req_pkgs <- c()
  files <- list.files(folder_path)
  for (file in files) {
    suppressWarnings(codes <- readtext(paste0(folder_path, "/", file)))
    pkgs <- stri_extract_all(codes$text, regex = "library\\(.*?\\)", simplify = TRUE)
    req_pkgs <- c(req_pkgs, as.vector(pkgs))
  }
  req_pkgs <- sub(".*\\(", "", req_pkgs)
  req_pkgs <- sub(")", "", req_pkgs)
  uninstalled_pkgs<- req_pkgs[! req_pkgs %in% rownames(installed.packages())]
  return(unique(uninstalled_pkgs))
}