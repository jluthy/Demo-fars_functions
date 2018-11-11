library(testthat)
library(fars)

test_that("fars_read() works correctly", {
  expect_that(fars_read(make_filename(111111)), throws_error())
})
