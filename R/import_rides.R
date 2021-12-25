#' Import multiple surveys
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{import_rides} imports all rideshare files in a given directory and
#' outputs an appended dataframe
#'
#' @param eval_directory the top level directory folder
#' @param filetype one of "csv", "rda", or "rds".
#' @param file_pattern a quoted regex expression to match file patterns in the
#'   directory
#' @return a dataframe of all appended survey files
#' @export
#' @import dplyr
#' @import haven
#' @import stringr
#' @import purrr
#' @import data.table

import_rides <- function(eval_directory,
                           filetype = "csv",
                           file_pattern = NULL) {

  files <- base::list.files(eval_directory,
                            pattern = file_pattern,
                            recursive = TRUE, # search all sub folders
                            full.names = TRUE, # list full file names
                            include.dirs = TRUE
  ) %>% # include the full file path
    dplyr::as_tibble() %>%
    dplyr::rename(paths = value) %>%
    dplyr::mutate(
      names = stringr::str_extract(basename(paths), ".")
    )


  # start with empty list
  file_list <- list()

  if (stringr::str_to_lower(filetype) == "csv") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- data.table::fread(y)
      }
    )
  } else if (stringr::str_to_lower(filetype) == "rds") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- base::readRDS(y)
      }
    )
  } else if (stringr::str_to_lower(filetype) == "rda") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- base::readRDS(y)
      }
    )
  } else {
    # return error
  }



  df <- dplyr::bind_rows(df)

  return(df)
}
