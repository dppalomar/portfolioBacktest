#' @title Evaluating multiple portfolio functions defined by customer
#'
#' @description Evaluate multiple portfolio functions written in format form
#'
#' @param folder_path path for a folder which contains all (and only) functions to be evaluated
#' @param portfolio_fun_list a list of portfolio functions, valid when \code{folder_path} is not passed
#' @param prices__ a list of \code{xts} containing the stock prices for the backtesting.
#' @param par_strategy an interger indicating number of strategies to be evaluated in parallel
#' @param packages a vector of strings indicating the required packages
#' @param return_all logical, indicating whether return all the results.
#' @param return_portfolio logical value, whether return portfolios.
#' @param shortselling whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage).
#' @param T_rolling_window length of the rolling window.
#' @param optimize_every how often the portfolio is to be optimized.
#' @param rebalance_every how often the portfolio is to be rebalanced.
#' @param cpu_time_limit time limit for executing portfolio function on a single data set
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
#' @export
multiplePortfolioBacktest <- function(folder_path = NULL, portfolio_fun_list = NULL, prices__, par_strategy = 1, packages = c(), return_all = FALSE, ...) {
  
  # check parallel setting
  par_strategy <- round(par_strategy)
  if (par_strategy < 1) stop("Parallel number must be positive interger")
  if (par_strategy > parallel::detectCores()) stop("Parallel number exceeds the hardware limit")
  
  if (is.null(folder_path) && is.null(portfolio_fun_list)) stop("The \"folder_path\" and \"portfolio_fun_list\" can not both be NULL")
  # when pass a list of function
  if (!is.null(portfolio_fun_list)) 
    return(multiplePortfoioBacktestPassFunctions(portfolio_fun_list, prices__, par_strategy, packages, return_all, ...))
  
  # extract useful informations
  files <- list.files(folder_path)
  stud_names <- stud_IDs <- c()
  for (i in 1:length(files)) {
    file <- files[i]
    file_name_cleaned <- gsub("\\s+", "", file)
    tmp <- unlist(strsplit(file_name_cleaned, "-|\\."))
    stud_names <- c(stud_names, paste(tmp[1], tmp[2], collapse = " "))
    stud_IDs <- c(stud_IDs, tmp[3])
  }
  
  # init all
  time_average <- rep(NA, length(files))
  failure_ratio <- rep(1, length(files))
  error_message <- list()
  portfolios_perform <- matrix(NA, length(files), 7)
  if (return_all) results_container <- list()
  
  # save the package and variables list
  packages_default <- search()
  var_fun_default <- ls()
  
  # some functions evaluation here
  
  if (par_strategy == 1) {
    for (i in 1:length(files)) {
      
      cat(paste0(Sys.time()," - Execute code from ", stud_names[i], " (", stud_IDs[i], ")\n"))
      
      # mirror list of present variables and functions
      var_fun_default <- ls()
      
      tryCatch({
        suppressMessages(source(paste0(folder_path, "/", files[i]), local = TRUE))
        # check if non-function variable is loaded
        if (checkNonFuncVar(setdiff(ls(), var_fun_default), env = environment()))
          stop("Non function variables are not allowed to be loaded.")
        
        res <- portfolioBacktest(portfolio_fun = portfolio_fun, prices = prices__, packages = packages, ...)
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
  } else {
    cl <- makeCluster(par_strategy)
    registerDoSNOW(cl)
    
    # creat the progress bar
    sink(file = tempfile())
    pb <- txtProgressBar(max = length(stud_IDs), style = 3)
    sink()
    
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    result <- foreach(file = files, .combine = c, .options.snow = opts) %dopar% {
      res__ <- list(source_error = FALSE)
      # snap the packages and variables
      packages_default <- search()
      var_fun_default <- ls()
      tryCatch({
        suppressMessages(source(paste0(folder_path, "/", file), local = TRUE))
        res__ <- c(res__, portfolioBacktest(portfolio_fun = portfolio_fun, prices = prices__, packages = packages, ...))
      }, warning = function(w){
        res__$source_error <<- TRUE
        res__$error_message <<- w$message
      }, error = function(e){
        res__$source_error <<- TRUE
        res__$error_message <<- e$message
      })
      
      # detach the newly loaded packages
      packages_now <- search()
      packages_det <- packages_now[!(packages_now %in% packages_default)]
      detach_packages(packages_det)
      
      # delete students' function
      var_fun_now <- ls()
      var_fun_det <- var_fun_now[!(var_fun_now %in% var_fun_default)]
      rm(list = var_fun_det)
      
      # return results
      return(list(res__))
    }
    
    close(pb)
    stopCluster(cl) 
    
    # extract result
    if (return_all) results_container <- result
    for (i in 1:length(files)) {
      error_message[[i]] <- result[[i]]$error_message
      if (result[[i]]$source_error) next
      portfolios_perform[i, ] <- result[[i]]$performance_summary
      time_average[i] <- result[[i]]$cpu_time_average
      failure_ratio[i] <- result[[i]]$failure_ratio
    }
    
  }
  
  error_message <- c(error_message, as.list(rep(NA, length(stud_IDs)-length(error_message))))
  rownames(portfolios_perform) <- names(time_average) <- names(failure_ratio) <- names(error_message) <- stud_IDs
  colnames(portfolios_perform) <- paste(c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio", "ROT bps"), " (median)")

  vars_tb_returned <- list("stud_names" = stud_names,
                           "stud_IDs" = stud_IDs,
                           "performance_summary" = portfolios_perform,
                           "cpu_time_average" = time_average,
                           "failure_ratio" = failure_ratio,
                           "error_message" = error_message)
  if (return_all) {
    results_container <- c(results_container, as.list(rep(NA, length(stud_IDs)-length(results_container))))
    names(results_container) <- stud_IDs
    vars_tb_returned$results_container <- results_container
  }
  return(vars_tb_returned)
}

multiplePortfoioBacktestPassFunctions <- function(portfolio_function_list, prices, par_strategy = 1, packages = c(), return_all, ...) {
  
  if (!is.list(portfolio_function_list)) stop("argument \"portfolio_function_list\" must be a list")
  
  n_function <- length(portfolio_function_list) 
  if (is.null(names(portfolio_function_list))) func_names <- paste0("func", 1:n_function)
  else func_names <- names(portfolio_function_list)
  
  # init all
  cpu_time_average <- rep(NA, n_function)
  failure_ratio <- rep(1, n_function)
  error_message <- list()
  performance_summary <- matrix(NA, n_function, 7)
  if (return_all) results_container <- list()
  
  if (par_strategy == 1) {
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
  } else {
    cl <- makeCluster(par_strategy)
    registerDoSNOW(cl)
    
    # creat the progress bar
    sink(file = tempfile())
    pb <- txtProgressBar(max = n_function, style = 3)
    sink()
    
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    result <- foreach(portfolio_fun = portfolio_function_list, .combine = c, .packages = packages, .options.snow = opts) %dopar% {
      return(list(portfolioBacktest(portfolio_fun = portfolio_fun, prices = prices, ...)))
    }
    
    close(pb)
    stopCluster(cl) 
    
    # extract result
    if (return_all) results_container <- result
    for (i in 1:n_function) {
      error_message[[i]]       <- result[[i]]$error_message
      performance_summary[i, ] <- result[[i]]$performance_summary
      cpu_time_average[i]      <- result[[i]]$cpu_time_average
      failure_ratio[i]         <- result[[i]]$failure_ratio
    }
  }
  rownames(performance_summary) <- names(cpu_time_average) <- names(failure_ratio) <- names(error_message) <- func_names
  colnames(performance_summary) <- paste(c("Sharpe ratio", "max drawdown", "annual return", "annual volatility", "Sterling ratio", "Omega ratio", "ROT bps"), " (median)")
  
  
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

checkNonFuncVar <- function(vars, env) {
  if (length(vars) == 0) return(FALSE)
  for (var in vars) {
    if (!is.function(get(var, envir = env))) return(TRUE)
  }
  return(FALSE)
}


#' @title Checking uninstalled packages written in the portfolio functions defined by customer
#'
#' @description Checke uninstalled packages of portfolio functions written in format form
#'
#' @param folder_path Path for a folder which contains all (and only) functions to be evaluated
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
checkUninstalledPackages <- function(folder_path, show_detail = FALSE) {
  if (!require("readtext")) stop("Package \"readtext\" is required to run this function!")
  if (!require("stringi")) stop("Package \"stringi\" is required to run this function!")
  uninstalled_pkgs_all <- c()
  files <- list.files(folder_path)
  for (file in files) {
    suppressWarnings(codes <- readtext(paste0(folder_path, "/", file)))
    pkgs <- stri_extract_all(codes$text, regex = "library\\(.*?\\)", simplify = TRUE)
    if (is.na(pkgs[1])) next
    pkgs <- as.vector(pkgs)
    pkgs <- sub(".*\\(", "", pkgs)
    pkgs <- sub(")", "", pkgs)
    uninstalled_pkgs<- pkgs[! pkgs %in% rownames(installed.packages())]
    uninstalled_pkgs_all <- c(uninstalled_pkgs_all, uninstalled_pkgs)
    
    if (show_detail) 
      if (length(as.vector(uninstalled_pkgs)) != 0)
        cat("find uninstalled packages", uninstalled_pkgs, "in", file, "\n")
  }
  return(unique(uninstalled_pkgs_all))
}

checKRequiredPackages <- function(file_path = NA, folder_path = NA, file_name = NA) {
  if (!require("readtext")) stop("Package \"readtext\" is required to run this function!")
  if (!require("stringi")) stop("Package \"stringi\" is required to run this function!")
  if (is.na(file_path)) file_path <- paste0(folder_path, "/", file_name)
  suppressWarnings(codes <- readtext(file_path))
  pkgs <- stri_extract_all(codes$text, regex = "library\\(.*?\\)", simplify = TRUE)
  if (is.na(pkgs[1])) return(c())
  else {
    pkgs <- as.vector(pkgs)
    pkgs <- sub(".*\\(", "", pkgs)
    pkgs <- sub(")", "", pkgs)
    return(pkgs)
  }
}