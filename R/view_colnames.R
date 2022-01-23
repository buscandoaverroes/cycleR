#' Get an overview of all csv column names in a directory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{view_colnames} imports the first n rows of each csv file in a given directory and reports column names
#'
#' @param eval_directory the top level directory folder
#' @param filetype "csv". Currently only csv extensions are supported.
#' @param file_pattern a quoted regex expression to match file patterns in the directory. Extensions must be .csv.
#' @param n_rows 10, the number of rows to import for each csv file.
#' @param check_names FALSE a logical vector passed on to \code{data.table::fread()} to check raw column names.
#' @return a dataframe of all appended survey files
#' @export
#' @import dplyr
#' @import haven
#' @import stringr
#' @import purrr
#' @import data.table
#' @import tidyr
#' @import janitor
#'
#'
#'
view_colnames <- function(eval_directory,
                          filetype = "csv",
                          file_pattern = "\\.csv",
                          n_rows = 10,
                          check_names = FALSE) {

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

    file_list[[x]] <<- data.table::fread(input = y,
                                         nrows = n_rows,
                                         check.names = check_names)  # T/F constant carried from import_stations

  }

  # import file into df list,
  # this function uses the helper function import_cols() from above and cycles through two lists
  # simultaneously: the file path and the name of the file defined in the columns of `files`
  df <- purrr::map2(
    files$names, files$paths, import_cols
  )

  # create a report using janitor
  janitor::compare_df_cols(df, return = "all")

}
