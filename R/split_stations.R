#' Removes station information and tables it for later.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{split_stations} removes station variables from a ride-level data-frame and returns a
#' list object containing the resulting trimmed dataframe and a second dataframe that contains
#' the unique station info that was extracted.
#'
#' @param df the dataframe that contains ride-level data.
#' @param col the string column containing member information.
#' @param keep should the original member column be preserved in the output?
#' @return a list object of two dataframes: the ride-level data with station info removed and the
#' unique station name-number combinations.
#' @export
#' @import dplyr
#'

split_stations <- function(df,
                           ...
                           ) {

  # user provided variables

  # separate station data from ride-level data
  rides <- df %>%
    dplyr::select(!tidyselect::all_of(...))

  stations <- df %>%
    dplyr::select(tidyselect::all_of(...)) %>%
    group_by(..1, ..2) %>%
    summarise() %>%
    filter(..1 != "") %>%   # remove blank entries
    ungroup() %>% group_by(..2) %>%
    arrange(..1) %>% # arrange by alpha within same group number
    mutate(id = row_number()) %>%
    pivot_wider(names_from = id, # pivot wider
                values_from = ..1)


    #dplyr::select({{ name_col_1 }}, {{ name_col_2 }}, {{ num_col_1 }}, {{ num_col_2 }})




    namenumb <- bks %>% # for start stations only
      group_by(start_name, start_number) %>%
      summarise() %>%
      filter(start_name != "") %>%   # remove blank entries
      ungroup() %>% group_by(start_number) %>%
      arrange(start_name) %>% # arrange by alpha within same group number
      mutate(id = row_number()) %>%
      pivot_wider(names_from = id, # pivot wider
                  values_from = start_name)


  return(stations)

}
