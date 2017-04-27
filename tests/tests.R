
library(farsfunctions)
library(testthat)

expect_that(fars_read_years(2018), gives_warning())

