
library(farsfunctions)


testthat::expect_that(class(fars_read_years('2013')), testthat::equals('list'))

testthat::expect_that(class(fars_summarize_years('2013')), testthat::equals(c('tbl_df', 'tbl', 'data.frame')))

testthat::expect_that(class(fars_map_state(21, 2013)), testthat::equals('NULL'))

