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
?dataset


##
## Developer commands (http://r-pkgs.had.co.nz/)
##
devtools::load_all()  #or Ctrl-Shift-L
devtools::install()
library(portfolioBacktest)
#devtools::build()  # to generate the installation file


# Documentation
devtools::document()  #to generate all documentation via roxygen
?portfolioBacktest
?dataset


# Code tests
devtools::test()
#covr::package_coverage()  #coverage of tests
#goodpractice::gp()  # overall checks

