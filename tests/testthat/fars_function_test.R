## Performs a test to check the output from the make_filename function
library(testthat)
library(dplyr)

test_that("fars_read() works correctly", {
  expect_is(fars_read("./inst/extdata/accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2016.csv.bz2"))
})
