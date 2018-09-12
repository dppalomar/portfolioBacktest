##
## User installation
##
# Local installation
install.packages(file.choose(), repos = NULL, type="source")
# Installation from GitHub
devtools::install_github("dppalomar/backtestPortfolio")
# Getting help
library(backtestPortfolio)
help(package = "backtestPortfolio")
package?backtestPortfolio
?backtestPortfolio
?prices


##
## Developer commands (http://r-pkgs.had.co.nz/)
##
devtools::load_all()  #or Ctrl-Shift-L
#devtools::use_package("mvtnorm")
devtools::install()
library(backtestPortfolio)
#devtools::build()  # to generate the installation file


# Documentation
devtools::document()  #to generate all documentation via roxygen
?backtestPortfolio
?prices
