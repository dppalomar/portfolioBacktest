context("Checking embedded dataset")

data("dataset10")  # data in the package
# to reduce required storage, only check sample mean and standard variance
# dataset_check <- sapply(dataset, function(x){sapply(x, function(x) {return(list(mean(x), mean(cov(x))))})})
# dataset_features_check <- unlist(dataset_check)
# save(dataset_features_check, file = "dataset_features_check.RData")
# load("dataset_features_check.RData")
load("dataset_features_check.RData")

test_that("the dataset used is the same", {
  dataset_check <- sapply(dataset10, function(x){sapply(x, function(x) {return(list(mean(x), mean(cov(x))))})})
  dataset_features <- unlist(dataset_check)
  expect_equal(dataset_features, dataset_features_check)
})

data("SP500_symbols")
# SP500symbols_check <- SP500_symbols
# save(SP500symbols_check, file = "SP500symbols_check.RData")

load("SP500symbols_check.RData")

test_that("the embedded SP500 symbols are the same", {
  expect_equal(SP500_symbols, SP500symbols_check)
})