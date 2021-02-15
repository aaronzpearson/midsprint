#' Acceleration Over Time
#'
#' @description
#' This function returns the rate of acceleration at a distinct duration. The player
#' must begin from velocity = 0 and run with maximal effort.
#'
#' @return Acceleration reached over time from zero velocity
#'
#' @param player_profile player profile from player_profile function
#' @param split_time time
#'
#' @noRd

accel_time <- function (player_profile, split_time)
{
  UseMethod("accel_time")
}


accel_time.default <- function(player_profile, split_time) {

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  acc_time <- (max_speed/player_tau) * exp(-split_time/player_tau)
  return(acc_time)

}
