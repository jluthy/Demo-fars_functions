#' fars functions for Coursera R package course
#'
#' The fars_read function imports a file, if it exists, in csv format and
#' converts it to a table. If file does not exist it returns an error.
#'
#' @param filename A character string indicating the name of csv file
#'
#' @return This function returns a table with data from the loaded file
#'
#' @examples \dontrun{
#' fars_read(filename="accident_2015.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @note Function will return error if file does not exist
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

#' This function creates the file name when given \code{year} as input
#'
#' @param year A number indicating the year
#'
#' @return This function returns a character string with coding "accident_year.csv.bz2"
#'
#' @examples \dontrun{
#' make_filename(2015)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' This function makes data frames with MONTH and year columns
#'
#' When passing the function a list of valid years as input, it gets associated files,
#' For each file it retrieves the corresponding data table and selects the MONTH and year columns.
#'
#' @param years A numeric vector representing years
#'
#' @return This function returns a list of tables corresponding to list of years.
#'
#' @importFrom dplyr mutate select
#'
#' @examples \dontrun{
#' fars_read_years(years=c(2013,2014,2015))
#' }
#'
#' @note Function will return an error if year is not valid
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

#' This function receives a list of years as an input and gets data tables
#' with \code{fars_read_years} Then it summarizes the results per year.
#'
#' @param years A numeric vector representing years
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a table with summarized results for each year.
#'
#' @importFrom dplyr bind_rows group_by summarize
#'
#' @importFrom tidyr spread
#'
#' @exmples \dontrun{
#' fars_summarize_years(years=c(2013,2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function gets passed a specific state and year, then gets associated data.
#' If the state exists, it filters the data per state and plots accidents for that state.
#'
#' @param state.num A number indicating a state
#'
#' @param year A number indicating the year
#'
#' @return This function creates a graph for a specific state and year
#'
#' @importFrom dplyr filter
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @examples \dontrun{
#' fars_map_state(state.num=1,year=2015)
#' }
#'
#' @note This function returns an error if the state number is not valid
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
