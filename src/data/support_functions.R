#
# Support functions to enrich and clean the Fish tracking data
#
# author:  Stijn Van Hoey & PieterJan Verhelst
#

#' Convert multiple columns as a date-column (posixct), given a specific format
#'
#' @param df data.frame
#' @param datecols (character) list of the columns to interpret
#' @param format (str) format string to interpret the dates
#'
convert_datecols <- function(df, datecols, format = "%Y-%m-%d %H:%M"){

    for (colname in datecols) {
        df[[colname]] <- as.POSIXct(df[[colname]], format, tz = "UCT")

        # check if the conversion provided all NA values and report if so
        if (sum(is.na(df[[colname]])) == nrow(df)) {
            stop("Date conversion resulted in all NA values!
                 Check the conversion format...")
        }
    }
    return(df)
}

#' Convert multiple columns as a date-column (posixct), given a specific format
#'
#' @param df data.frame
#' @param datecol (character) date column to interpret year and month from
#' @param prefix (char) prefix to add to the month and year column names
#'
add_yearmonth <- function(df, datecol, name_prefix = ""){
    df[[paste(name_prefix, "month",
              sep = "")]] <- lubridate::month(df[[datecol]])
    df[[paste(name_prefix, "year",
              sep = "")]] <- lubridate::year(df[[datecol]])
    return(df)
}