#' Removes station information and tables it for later.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{split_stations} removes station variables from a ride-level data-frame and returns a
#' list object containing the resulting trimmed dataframe and a second dataframe that contains
#' the unique station info that was extracted.
#'
#' @param df the dataframe that contains ride-level data.
#' @param start_name a character vector of names of columns that may contain start name data
#' @param start_no a character vector of names of columns that may contain start number data
#' @param end_name a character vector of names of columns that may contain end name data
#' @param end_no a character vector of names of columns that may contain end number data
#' @param return_list FALSE, should the return object include the removed station info?
#' @param preserve_numbers TRUE, should the columns containing the start and end station numbers be maintained?
#' @return a list object of two dataframes: the ride-level data with station info removed and the
#' unique station name-number combinations.
#' @export
#' @import dplyr
#' @import tidyselect
#'

split_stations <- function(df,
                           start_name = "start",
                           start_no = "startno",
                           end_name = "end",
                           end_no = "endno",
                           return_list = FALSE,
                           preserve_numbers = TRUE
                           ) {


  # separate rides and stations
  stations <- df %>%
    dplyr::select(tidyselect::any_of(c({{ start_name }}, {{ start_no }}, {{ end_name }}, {{ end_no }})))

  if (preserve_numbers) {

    rides <- df %>%
      dplyr::select(!tidyselect::any_of(c({{ start_name }}, {{ end_name }})))

  } else {

    rides <- df %>%
      dplyr::select(!tidyselect::any_of(c({{ start_name }}, {{ start_no }}, {{ end_name }}, {{ end_no }})))

    }


  # optionally return the ride data only or rides and stations as a list
  if (return_list) {
    data <- list("rides" = rides, "stations" = stations)
  } else {
    data <- rides
  }


  return(data)

}
