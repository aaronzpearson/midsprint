#' Speed Acceleration Values
#'
#' Returns the initial values utilized in the speed_accel_model() function.
#'
#' @param game_data
#'
#' @description
#' This function is a helper function and is not exported to the NAMESPACE
#'
#' @return Back-end values fed into speed_accel_model()
#'
#' @noRd
speed_accel_values <- function (game_data)
{
  UseMethod("speed_accel_values")
}


speed_accel_values.default <- function(game_data) {

  df <- game_data

  player_temp <- data.frame(
    df$speed,
    df$accel
  )

  player_temp <- player_temp[player_temp[, "df.speed"] >= 3, ]
  player_temp$vel_bins <- cut(player_temp$df.speed, seq(3, max(df$speed), by = 0.2))

  player_temp <- player_temp[order(player_temp$df.accel), ]
  temp <- by(player_temp, player_temp["vel_bins"], tail, 2)
  player_temp <- Reduce(rbind, temp)

  player_temp

}
