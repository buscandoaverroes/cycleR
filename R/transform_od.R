#' Transform ride-level data into OD data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{transform_od} takes ride-level data in a data.frame and transforms the data into
#' origin-destination data by summarizing at a user-defined time interval
#'
#' @param df the dataframe that contains ride-level data.
#' @param origin_datetime the column containing origin/departure date-time information.
#' @param destination_datetime the column containing destination/arrival date-time information.
#' @param timezone "US/Eastern", the timezone passed to \code{lubridate}. See \code{base::OlsonNames()} for valid tz names.
#' @param weekstart 7, an integer passed to \code{base::getOption('lubridate.week.start')}. Determines the start day of the week. 7 = Sunday.
#' @param member_var member, an unquoted name of the variable containing the string member data.
#' @param group_vars a character vector of additional variables to determine origin-destination groups.
#' Defaults to group by start/end station and year-month-day-hour.
#' @param station_vars a character vector of variable names that define the unique stations.
#' @param round_dig 3, the number of digits when rounding.
#' @param granularity NULL one of "hour", "day", or "month". The precision to which the data will be collapsed.
#' @return a dataframe of summarized origin-destination information.
#' @export
#' @import dplyr
#' @import lubridate
#' @import stats


transform_od <- function(df,
                         origin_datetime,
                         destination_datetime,
                         timezone = "US/Eastern",
                         weekstart = 7,
                         member_var,
                         group_vars = c("year", "month", "hour", "day_of_wk", "day_of_yr"),
                         station_vars = c("startno", "endno"),
                         round_dig  = 3,
                         granularity = NULL # hour, day, week, month, year
                         ) {

  gran <- dplyr::case_when(
    granularity == "hour" ~ c("year", "month", "hour", "day_of_wk", "day_of_yr"),
    granularity == "day"  ~ c("year", "month", "day_of_wk", "day_of_yr"),
    granularity == "month" ~ c("year", "month")
  )


  # determine groups + member var
  by <- rlang::syms(c(group_vars, station_vars))
  mem <- rlang::sym(member_var)


  # add lubridate data for year, month, hour, etc
  df2 <- df %>%
    dplyr::mutate( # generate components of duration
      leave  = lubridate::ymd_hms({{ origin_datetime }}, tz = timezone),
      arrive = lubridate::ymd_hms({{ destination_datetime }}, tz = timezone)
    ) %>%
    dplyr::mutate(
      dur   = dplyr::if_else(base::is.na(duration), # make duration, check for existing variable called "duration"
                      true = base::as.integer(base::round(lubridate::interval(leave, arrive))),
                      false = base::as.integer(base::round(duration))),
      year  = base::as.integer(lubridate::year(leave)),
      month = lubridate::month(leave, label = FALSE), # leave as numeric
      hour  = base::as.integer(lubridate::hour(leave)),
      day_of_wk  = base::as.integer(lubridate::wday(leave,
                                                    label = FALSE,
                                                    week_start=base::getOption('lubridate.week.start', weekstart))), # numeric, start sunday
      day_of_yr = base::as.integer(lubridate::yday(leave))
    )


  # summarize by specified granularity and transform to OD data
  df3 <- df2 %>%
    dplyr::group_by( !!!by ) %>% # !!!by
    dplyr::summarise(
      dur_med = base::round(stats::median(dur, na.rm = TRUE), round_dig),
      dur_sd  = base::round(stats::sd(dur, na.rm = TRUE), round_dig),
      member_pct = base::round(base::mean(!!mem), round_dig), #!!!mem
      n_rides = n()
    )


  return(df3)

}
