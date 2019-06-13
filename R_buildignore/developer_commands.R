##
## User installation
##
# Local installation
install.packages(file.choose(), repos = NULL, type="source")
# Installation from GitHub
devtools::install_github("dppalomar/portfolioBacktest")
# Installation from CRAN
install.packages("portfolioBacktest")
# Getting help
library(portfolioBacktest)
help(package = "portfolioBacktest")
package?portfolioBacktest
?portfolioBacktest
?dataset10
citation("portfolioBacktest")
vignette(package = "portfolioBacktest")


##
## Developer commands (http://r-pkgs.had.co.nz/)
##
devtools::load_all()  #or Ctrl-Shift-L
devtools::install()
library(portfolioBacktest)

# Documentation
devtools::document()  #to generate all documentation via roxygen
?portfolioBacktest
?dataset10


# Code tests
devtools::test()
#covr::package_coverage()  #coverage of tests


# CRAN check and submission (http://r-pkgs.had.co.nz/release.html)
#  checklist: https://kalimu.github.io/post/checklist-for-r-package-submission-to-cran/
devtools::check()
rcmdcheck::rcmdcheck()
devtools::build()
#devtools::revdep(pkg = "riskParityPortfolio")  # to check reverse dependencies
#devtools::build_win()  #to check under windows
#R CMD build .  # this is to generate tarball
#R CMD check portfolioBacktest_0.1.0.tar.gz --as-cran  # this is before submission to CRAN
#R CMD install portfolioBacktest_0.1.0.tar.gz
#submit the tarball directly via the webform: https://cran.r-project.org/submit.html
