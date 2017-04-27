
library(farsfunctions)
library(testthat)

expect_that(make_filename(2014), is_identical_to("accident_2014.csv.bz2"))
expect_that(fars_read_years(2018), gives_warning())

