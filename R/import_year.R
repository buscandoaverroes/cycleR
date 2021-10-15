#' @title Import data in year format
#' @param file The path to the file to import
#' @param rename.list A named vector of new names from old imported names
#' @param na.strings Passed on to `fread()`; how to interpret missing cells
#' @param col.types Passed on to colClasses of `fread()`; named or unnamed vector to specify col types.
#' @return An imported file as a tibble
#' @importFrom data.table fread
#' @import dplyr
#' @import tidyselect
#' @import magrittr
#' @import tibble

import_year <- function(file,
                        rename.list = NULL,
                        na.strings = "",
                        col.types = NULL) {


  # check that rename.list has a valid names attribute, if defined
  if ( all(c(!is.null(rename.list), is.null(attr(rename.list, "names")))) ) {
    stop("rename.list must be a named vector or NULL")
  }



  # import the file
  year <- data.table::fread(file = file,
                            na.strings = na.strings,
                            colClasses = col.types)

  # if rename.list !null, rename
  if (!is.null(rename.list)) {
    year <- year %>%
      dplyr::rename(dplyr::all_of(rename.list))
  }

  # subset cols
  year <- year %>%
    dplyr::select(base::names(rename.list), tidyselect::everything())



  year

}
