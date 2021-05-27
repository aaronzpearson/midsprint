#' Conversion Factor
#'
#' @param player_profile A data.frame built using the game_profile or optim_profile functions.
#'
#' @return Converts user input units to metric.
#'
#' @description
#'
#' Units are extracted from the player's game_data function that are stored in their player profile. Values are returned in their original inputs.
#' Converting values to metric allows back-end functions to model a player's mechanical sprint qualities in keeping with their intended usage.
#' @export
conversion_factor <- function (value, units = "m/s") {

  UseMethod("conversion_factor")

}

#' @export
conversion_factor.default <- function(value, units = "m/s") {

  # converts values to metric
  # in m/s

  # functions create objects (to_metric & from_metric)
  # from_metric = 1/to_metric
  distance <- to_meters(value, strsplit(units, "/")[[1]][1])
  duration <- to_seconds(strsplit(units, "/")[[1]][2])

  distance/ duration

}

# helper functions --------------------------------------------------------

# These functions feed into conversion_factor() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

metric <- function(player_profile) {

  # extracts units from player_profile
  paste0(player_profile[[4]], "/", player_profile[[5]])

}

to_meters <- function(value, units = c("ft", "km", "m", "mi", "yd")) {

  # converts ft, km, mi, yd to meters
  # provides some leeway on different spelling of each input

  if(units %in% c("meters", "m", "meter")) {
    return(value)

  } else if(units %in% c("feet", "ft", "fts", "f")) {
    return(value * 0.3048)

  } else if(units %in% c("kilometers", "kilometer", "k", "km")) {
    return(value * 1000)

  } else if(units %in% c("miles", "mile", "mi")) {
    return(value * 1609.344)

  } else if(units %in% c("yard", "yards", "yd", "yds")) {
    return(value * 0.9144)

  }

}

to_seconds <- function(units = c("second, minute, hour")) {

  # converts min and hours to seconds

  if(units %in% c("seconds", "second", "s", "sec")) {
    1

  } else if(units %in% c("min", "m", "minute")) {
    1*60

  } else if(units %in% c("hour", "h", "hr")) {
    1*60*60

  }

}
