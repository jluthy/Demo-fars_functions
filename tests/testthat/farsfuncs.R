## Performs a test to check the output from the make_filename function
library(testthat)
test_that("filename is correct", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
})
