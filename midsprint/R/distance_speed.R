#' Distance Reached at a Given Speed
#'
#' @param player_profile Player's f-v-p profile
#' @param current_speed Player's current speed
#'
#' @return The distance a player can run to reach a given speed from a velocity >= 0
#'
distance_speed <- function(player_profile, current_speed) {

  df <- player_profile
  max_speed <- as.numeric(df[1])
  player_tau <- as.numeric(df[3])

  split_time <- time_speed(max_speed, current_speed, player_tau)

  pos_vel <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - (max_speed * player_tau)
  return(pos_vel)

}
