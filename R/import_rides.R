#' Import multiple surveys
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{import_rides} imports all rideshare files in a given directory and
#' outputs an appended dataframe
#'
#' @param eval_directory the top level directory folder
#' @param filetype "csv". Currently only csv extensions are supported.
#' @param file_pattern a quoted regex expression to match file patterns in the directory. Extensions must be .csv.
#' @param rename_cols a vector of new names passed on to \code{data.table::fread()} to rename columns.
#' @param check_names TRUE a logical vector passed on to \code{data.table::fread()} to check raw column names.
#' @param nrows Inf passed on to \code{data.table::fread()}. Either Inf or numeric value
#' @return a dataframe of all appended survey files
#' @export
#' @import dplyr
#' @import haven
#' @import stringr
#' @import purrr
#' @import magrittr
#' @import data.table

import_rides <- function(eval_directory,
                         filetype = "csv",
                         file_pattern = "\\.csv",
                         rename_cols = NULL,
                         nrows = Inf,
                         check_names = TRUE) {

  # check file extension supplied
 # write check that only .csv files will be supplied


  # determine files to import
  files <- base::list.files(eval_directory,
                            pattern = "\\.csv$",
                            recursive = TRUE, # search all sub folders
                            full.names = TRUE, # list full file names
                            include.dirs = TRUE) %>% # include the full file path
    dplyr::as_tibble() %>%
    dplyr::rename(paths = value) %>%
    dplyr::mutate(
      names = base::basename(paths)
    )


  # start with empty list
  file_list <- list()

  # import file into df list
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- data.table::fread(input = y, check.names = check_names, nrows = nrows) %>%
          rename(any_of(rename_cols))
          }
    )



  # bind all rows of df
  df2 <- dplyr::bind_rows(df)

  return(df2)
}
