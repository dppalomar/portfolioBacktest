##
## User installation
##
# Local installation
install.packages(file.choose(), repos = NULL, type="source")
# Installation from GitHub
devtools::install_github("dppalomar/portfolioBacktest")
# Getting help
library(portfolioBacktest)
help(package = "portfolioBacktest")
package?portfolioBacktest
?portfolioBacktest
?prices


##
## Developer commands (http://r-pkgs.had.co.nz/)
##
devtools::load_all()  #or Ctrl-Shift-L
#devtools::use_package("xts")
devtools::install()
library(portfolioBacktest)
#devtools::build()  # to generate the installation file


# Documentation
devtools::document()  #to generate all documentation via roxygen
?portfolioBacktest
?prices


# Code tests
#devtools::use_testthat()  # the first time
devtools::test()
#covr::package_coverage()  #coverage of tests
#goodpractice::gp()  # overall checks

