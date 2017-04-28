library(dplyr)
library(maps)
library(graphics)
library(stats)


#' Read in NHTSA FARS dataset CSV file as a data frame table (tbl_df) object
#'
#' This is a function that takes a filename of a CSV as an argument and reads it in
#' as a data frame table. Successful execution will result in a data frame table
#' object containing the CSV data.
#'
#' @details It will first check if the filename is valid and print a
#' warning and stop if it is not. The function will generate an error for invalid CSV
#' inputs.
#'
#' @param filename A string filename that corresponds to the CSV to be read in
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return This function returns a tbl_df object
#'
#' @examples
#' \dontrun{
#' fars_read('accident_2013.csv.bz2')
#' }
#' @export

fars_read <- function(filename) {
          if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Print a filename based on the value of the year argument passed to the function
#'
#' This is a function that takes a year in the form of an integer or string as an argument
#' and then uses that value to print a standard filename for data of that given year.
#'
#' @details Errors: This function will result in errors to the functions that call it as a helper if values other
#' years or other than years for which there is data are passed.
#'
#' @param year A year passed as an argument in the form of an integer or string for which
#' the filename should be labeled and formatted
#'
#'
#' @return This function returns a string
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename('2013')
#' }
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Return the month and year corresponding to each FARS observation
#'
#' This is a function that takes a year in the form of an integer or string as an argument
#' and then uses that value to read in a CSV as a tbl_df for data of that given year and
#' and returns the month and year corresponding to each observation.
#'
#' @details This function will return an error message for invalid years.
#'
#' @param years A year passed as an argument in the form of an integer or string for which
#' the data should be read in
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom stats setNames
#'
#' @return This function returns a list
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years('2013')
#' }
#'
#' @export

fars_read_years <- function(years) {
        year <- NULL
        MONTH <- NULL
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Return a monthly summary of incidents
#'
#' This is a function that takes a year in the form of an integer or string as an argument
#' and then uses that value to read in a CSV as a tbl_df for data of that given year and
#' and returns a monthly summary of the observations.
#'
#' @details This function will return an error message for invalid years.
#'
#' @param years A year passed as an argument in the form of an integer or string for which
#' the data should be read in.
#'
#' @importFrom dplyr %>% group_by bind_rows summarize
#' @importFrom tidyr spread
#' @importFrom stats setNames
#'
#'
#' @return This function returns a tbl_df object
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years('2013')
#' }
#'
#' @export

fars_summarize_years <- function(years) {
        MONTH <- NULL
        n <- NULL
        year <- NULL
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Return a map plot of incidents.
#'
#' This is a function that takes a state number and year in the form of an integer or string as
#' an argument and then uses that value to generate a map plot of observations for that state.
#' and year.
#'
#' @details This function will return an error message for invalid years, invalid state numbers,
#' and if there are no accidents to plot
#'
#' @param state.num A numeric or string that corresponds to a
#' given state in the data set.
#' @param year A year passed as an argument in the form of an integer or string for which
#' the data should be read in.
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#'
#' @return This function returns a plot of observations in the selected state will NULL
#' class
#'
#' @examples
#' \dontrun{
#' fars_map_state(20, 2013)
#' fars_map_state('20','2013')
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
        STATE <- NULL
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
