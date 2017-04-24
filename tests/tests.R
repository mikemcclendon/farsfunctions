library(testthat)
library(farsfunctions)


expect_that(class(fars_read_years('2013')), equals('list'))

expect_that(class(fars_summarize_years('2013')), equals(c('tbl_df', 'tbl', 'data.frame')))

expect_that(class(fars_map_state(21, 2013)), equals('NULL'))

