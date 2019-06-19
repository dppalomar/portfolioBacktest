#
# Checking uninstalled packages written in the portfolio functions defined by customer
#
# checkUninstalledPackages <- function(folder_path, show_detail = FALSE) {
#   if (!requireNamespace("readtext", quietly = TRUE)) stop("Package \"readtext\" is required to run this function!")
#   if (!requireNamespace("stringi", quietly = TRUE)) stop("Package \"stringi\" is required to run this function!")
#   uninstalled_pkgs_all <- c()
#   files <- list.files(folder_path)
#   for (file in files) {
#     suppressWarnings(codes <- readtext::readtext(paste0(folder_path, "/", file)))
#     pkgs <- stringi::stri_extract_all(codes$text, regex = "library\\(.*?\\)", simplify = TRUE)
#     if (is.na(pkgs[1])) next
#     pkgs <- as.vector(pkgs)
#     pkgs <- sub(".*\\(", "", pkgs)
#     pkgs <- sub(")", "", pkgs)
#     uninstalled_pkgs<- pkgs[! pkgs %in% rownames(utils::installed.packages())]
#     uninstalled_pkgs_all <- c(uninstalled_pkgs_all, uninstalled_pkgs)
#     
#     if (show_detail) 
#       if (length(as.vector(uninstalled_pkgs)) != 0)
#         message("find uninstalled packages ", uninstalled_pkgs, " in ", file, "\n")
#   }
#   return(unique(uninstalled_pkgs_all))
# }


# detach the loaded packages from current environment
detachPackages <- function(items) {
  for (item in items) {
    if (item %in% search()) {
      detach(item, unload = TRUE, character.only = TRUE)
    }
  }
}