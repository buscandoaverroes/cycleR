#' Import unique bikeshare stations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{import_stations} imports station info from a directory of files and appends all unique
#' number combinations
#'
#' @param eval_directory the top level directory folder
#' @param filetype "csv". Currently only csv extensions are supported.
#' @param station_cols a list of possible columns to check for station data. Alternatively, tidyselect::contains("station")
#' @param file_pattern a quoted regex expression to match file patterns in the directory. Extensions must be .csv.
#' @param rename_expr an expression of `new_name` = `old_name` for columns to rename.
#' @param check_names TRUE a logical vector passed on to \code{data.table::fread()} to check raw column names.
#' @return a dataframe of all appended survey files
#' @export
#' @import dplyr
#' @import haven
#' @import stringr
#' @import purrr
#' @import data.table
#'
#'
#'
import_stations <- function(eval_directory,
                           filetype = "csv",
                           file_pattern = "\\.csv",
                           station_cols,
                           rename_expr = rlang::expr(c(
                             station_name = tidyselect::ends_with("station"),
                             station_no   = tidyselect::ends_with("number")
                             )),
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


  # write import function for year file, where "variable" is the file path
  import_cols <- function(x, y) {

    file_list[[x]] <<- data.table::fread(input = y, check.names = check_names) %>% # T/F constant carried from import_stations
      dplyr::select(tidyselect::any_of({{ station_cols }})) %>% # list constant carried from import_stations
      dplyr::rename(rlang::eval_tidy(rename_expr))
      #dplyr::rename_with({{ old_number_fn}}, "new_station_no")
  }

  # import file into df list,
  # this function uses the helper function import_cols() from above and cycles through two lists
  # simultaneously: the file path and the name of the file defined in the columns of `files`
  df <- purrr::map2(
    files$names, files$paths, import_cols
  )



  # bind all rows of df
  df2 <- dplyr::bind_rows(df)

  return(df2)
}
