#' @title Evaluating multiple portfolio functions defined by customer
#'
#' @description Evaluate multiple portfolio functions written in format form
#'
#' @param path absolute path for a folder which contains all (and only) functions to be evaluated
#' @param prices a list of \code{xts} containing the stock prices for the backtesting.
#' @param shortselling whether shortselling is allowed or not (default \code{FALSE}).
#' @param leverage amount of leverage (default is 1, so no leverage).
#' @param T_sliding_window length of the sliding window.
#' @param freq_optim how often the portfolio is to be reoptimized.
#' @param freq_rebalance how often the portfolio is to be rebalanded.
#' @return A list containing the performance in the following elements:
#' \item{\code{TBD}  }{m-by-m matrix, columns corresponding to eigenvectors.}
#' \item{\code{TBD}  }{m-by-1 vector corresponding to eigenvalues.}
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @import xts, PerformanceAnalytics
#' @export
multiplePortfolioFunEval <- function(path, prices,
                              shortselling = FALSE, leverage = 1,
                              T_sliding_window = 6*21, freq_optim = 5, freq_rebalance = freq_optim) {
  # extract useful informations
  files <- list.files(path)
  stud_names <- stud_IDs <- eval_time <- c()
  portfolio_perform <- warning_info <- error_info <- list()
  
  # save the package and variables list
  packages_default <- search()
  var_fun_default <- ls()
  print("---------------Default Packages---------------")
  print(packages_default)
  print("----------------------------------------------")
  # some functions evaluation here
  for (i in 1:length(files)) {
    
    file <- files[i]
    file_name_cleaned <- gsub("\\s+", "", file)
    tmp <- unlist(strsplit(file_name_cleaned, "-"))
    stud_names <- c(stud_names, paste(tmp[1], tmp[2], collapse = " "))
    stud_IDs <- c(stud_IDs, tmp[3])
    print(paste0(Sys.time()," - Execute code from ", stud_names[i], " (", stud_IDs[i], ")"))
    
    # mirror list of present variables and functions
    var_fun_default <- ls()
    
    # evaluate code
    success_flag <- TRUE
    start.time <- Sys.time() # timing the backtest evaluation time
    res = tryCatch({
      # source file
      suppressMessages(source(paste0(path, "/", file), local = TRUE))
      
      # do evaluation
      multipleBacktestPortfolio(portfolio_fun = portfolio_fun, 
                                prices = prices,
                                shortselling = shortselling, 
                                leverage = leverage, 
                                T_sliding_window = T_sliding_window, 
                                freq_optim = freq_optim, 
                                freq_rebalance = freq_rebalance)
    }, warning = function(w) {
      success_flag <- FALSE
      warning_info[[i]] <- w
      print(w)
    }, error = function(e) {
      success_flag <- FALSE
      print(e)
      error_info[[i]] <- e
    }, finally = {
    })
    end.time <- Sys.time()
    eval_time <- c(eval_time, as.numeric(end.time - start.time))
    
    # deal with result, use midean
    if (success_flag) print("No errors! No warnings!")
    try(portfolio_perform[[i]] <- apply(res$performance, 1, median))
    
    
    # detach the newly loaded packages
    packages_now <- search()
    packages_det <- packages_now[!(packages_now %in% packages_default)]
    detach_packages(packages_det)
    
    # delete students' function
    var_fun_now <- ls()
    var_fun_det <- var_fun_now[!(var_fun_now %in% var_fun_default)]
    rm(list = var_fun_det)
  }
  
  return(list(
    "stud_names" = stud_names,
    "stud_IDs" = stud_IDs,
    "performance" = portfolio_perform,
    "eval_time" = eval_time,
    "warnings" = warning_info,
    "errors" = error_info
  ))
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
#' @param path Absolute path for a folder which contains all (and only) functions to be evaluated
#' 
#' @author Daniel P. Palomar and Rui Zhou
#' 
#' @import readtext, stringi
#' @export
checkUninstalledPackages <- function(path) {
  req_pkgs <- c()
  files <- list.files(path)
  for (file in files) {
    suppressWarnings(codes <- readtext(paste0(path, "/", file)))
    pkgs <- stri_extract_all(codes$text, regex = "library\\(.*?\\)", simplify = TRUE)
    req_pkgs <- c(req_pkgs, as.vector(pkgs))
  }
  req_pkgs <- sub(".*\\(", "", req_pkgs)
  req_pkgs <- sub(")", "", req_pkgs)
  uninstalled_pkgs<- req_pkgs[! req_pkgs %in% rownames(installed.packages())]
  return(unique(uninstalled_pkgs))
}