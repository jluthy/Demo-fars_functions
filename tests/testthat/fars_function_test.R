## Performs a test to check the output from the make_filename function
library(testthat)
library(dplyr)

test_that("fars_read() works correctly", {
  expect_that(fars_read(make_filename(111111)), throws_error())
})

