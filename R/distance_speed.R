#' Distance Reached at a Given Speed
#'
#'@description
#'This function returns the distance a player travels
#'
#' @param speed speed of interest
#' @param player_profile player's f-v-p profile
#'
#' @return The distance a player must run to reach a given speed from a velocity >= 0
#'
#' @noRd
distance_speed <- function (player_profile, current_speed)
{
  UseMethod("distance_speed")
}


distance_speed.default <- function(player_profile, current_speed) {

  df <- player_profile
  max_speed <- df[[1]]
  player_tau <- df[[3]]

  split_time <- time_speed(player_profile, current_speed)

  pos_vel <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - (max_speed * player_tau)
  return(pos_vel)

}
