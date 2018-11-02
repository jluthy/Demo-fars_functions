## Performs a test to check the output from the make_filename function
library(testthat)
expect_that(farsfuncs::make_filename(2015),equals("accident_2015.csv.bz2"))