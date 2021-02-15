#' Player Game Tracking Data
#'
#' This function returns a player's tracking data data frame.
#'
#'@description
#' game_data() is included in the **midsprint** work flow to provide coaches with a simple method
#' of cleaning positional tracking data. It also reduces overhead associated with large data sets. Doing
#' so allows the **midsprint** package to run quickly, smoothly, and efficiently.
#'
#' @param speed speed vector from raw data
#' @param acceleration acceleration vector from raw data
#'
#' @return Returns a simplified data frame containing an athlete's speed and acceleration values
#' @export
#'
#' @examples
#' data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' head(player_a_data)
game_data <- function (speed, acceleration)
{
  UseMethod("game_data")
}

#' @export

game_data.default <- function(speed, acceleration) {

  df <- data.frame(speed = speed,
                   accel = acceleration)

  colnames(df) <- c("speed", "accel")


  return(df)

}
