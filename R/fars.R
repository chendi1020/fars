#' read in csv data
#' 
#' This is a simple function that checks for existence of a csv file and . If exists, read in csv file by its name. If not, throw an erro
#' 
#' @param filename A character string with a route to the csv file that needs to be read
#' 
#' @return This function return a tibble object of the csv file data
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @examples 
#' \dontrun{fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "fars"))}
#' 
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make filename 
#' 
#' This function makes it a custom file name based on year given
#' 
#' @param year a 4-digt year to format filename
#' 
#' @return a string with custom year filename
#' 
#' @example 
#' \dontrun{make_filename(2014)}
#' 
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' batch import csv file
#' 
#' Iterate over a year vector that read csv data for each given year. if the csv file for that year does not exist, throw a warning.
#' If exist, create a year column and select only year and month columns
#' 
#' @param  years a vector of years
#' 
#' @return A list of tibbles. Each tibble has 2 columns- month and year
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' \dontrun{fars_read_years(2013:2015)}
#' 
#' @export
fars_read_years <- function(years) {
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

#' summarize by fars year
#' 
#' Count of number of observartion by month in the given years
#' 
#' @param years a numeric vectors for years
#' 
#' @return a wide tibble. one column for year and all other columns are month. Values are the count of oberservations by month in a year row
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @examples 
#' \dontrun{fars_summarize_years(2013:2015)}
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Draw a data map for a given year and state
#' 
#' This function read in a fars data file for a given year, and take a state.num. If state.num exist, it draws a map for a given year and the state
#' If state.num does not exist, throw a error. If there is no observation in the given year and state, throw a message
#' 
#' @param state.num a numeric number
#' @param year a numeric number to indicate year -4 digt
#' 
#' @return an error if the state.num given doesn't exist. a message if there is no oberservation in the year and state.num given. Otherwise, a map
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples 
#' \dontrun{fars_map_state(1,2015)}
#' 
#' @export
fars_map_state <- function(state.num, year) {
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