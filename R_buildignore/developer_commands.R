##
## User installation
##
# Local installation
install.packages(file.choose(), repos = NULL, type = "source")
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
## Developer commands (https://r-pkgs.org/)
##
devtools::load_all()  #or Ctrl-Shift-L
devtools::document()  #to generate all documentation via roxygen
devtools::install()
library(portfolioBacktest)
#tools::showNonASCIIfile("portfolioBacktest.R")


# Code tests
devtools::test()
#covr::package_coverage()  #coverage of tests
devtools::run_examples(test = TRUE)
pkgload::run_example("man/backtestLeaderboard.Rd", run_donttest = TRUE)


# Reverse dependencies
devtools::revdep(pkg = "portfolioBacktest")
revdepcheck::revdep_check(num_workers = 4)  # https://github.com/r-lib/revdepcheck


# CRAN check and submission (https://r-pkgs.org/release.html)
#  checklist: https://kalimu.github.io/post/checklist-for-r-package-submission-to-cran/
devtools::check()  # run_dont_test = TRUE
rcmdcheck::rcmdcheck()  # build_args = "--run-donttest"
devtools::build()
#devtools::check_win_release()  #to check under windows
#R CMD build .  # this is to generate tarball
#R CMD check portfolioBacktest_0.3.1.tar.gz --as-cran --run-donttest  # this is before submission to CRAN
#R CMD install portfolioBacktest_0.3.1.tar.gz
#submit the tarball directly via the webform: https://cran.r-project.org/submit.html
