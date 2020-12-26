#' Acceleration Over Time
#'
#' @return Acceleration reached over time from zero velocity
#'
#' @param max_speed Max observed speed of the player
#' @param player_tau A unique Tau value for each player
#' @param split_time Time
#'
accel_time <- function(max_speed, player_tau, split_time) {

  acc_time <- (max_speed/player_tau) * exp(-split_time/player_tau)
  return(acc_time)

}
