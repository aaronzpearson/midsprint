#' Acceleration Over Time
#'
#' @return Acceleration reached over time from zero velocity
#'
#' @param player_profile player profile from player_profile function
#' @param split_time Time
#'
accel_time <- function(player_profile, split_time) {

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  acc_time <- (max_speed/player_tau) * exp(-split_time/player_tau)
  return(acc_time)

}
