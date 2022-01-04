#' Cleans member data stored in string variable
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{clean_member} takes ride-level data in a data.frame and turns the member data stored in a string
#' variable into a binary column.
#'
#' @param df the dataframe that contains ride-level data.
#' @param col the string column containing member information.
#' @param keep should the original member column be preserved in the output?
#' @return a dataframe of all ammended member information in `member`.
#' @export
#' @import dplyr
#'


clean_member <- function(df,
                         col = "member",
                         keep = FALSE,
                         true = c("Member"),
                         false = c("guest", "Casual", "Unknown")
                         ) {

  # store names of provided df
  names <- base::names(df)
  exists <- "member" %in% names


  # make user-supplied column a symbol to eval later with !!
  var <- sym(col)


  # create a new binary column called "member" but only if original cols don't contain this name.
  # Else make "member_new"
  if (exists) {
    new_col <- "member_new"
  } else {
    new_col <- "member"
  }

  df2 <- df %>%
    dplyr::mutate(
      {{new_col}} := dplyr::case_when(  # clean/determine member now to determine member ratio in df3
        base::tolower(!!var) %in% base::tolower(true) ~ TRUE,
        base::tolower(!!var) %in% base::tolower(false) ~ FALSE
      )
    )


  if (keep) {
    df3 <- df2
  } else {
    if (exists) {
      df3 <- df2 %>%
        dplyr::select(-{{ col }}) %>%
        rename("member" = "member_new")
    } else {
      df3 <- df2 %>%
        dplyr::select(-{{ col }})
    }
  }

  return(df3)

}
