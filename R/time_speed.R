#' Time Speed
#'
#' @param player_profile player profile
#' @param current_speed the player's current speed
#'
#' @return How long it takes to reach max speed with a starting speed >= 0
#'
#' @noRd
time_speed <- function (player_profile, current_speed)
{
  UseMethod("time_speed")
}


time_speed.default <- function(player_profile, current_speed) {
  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  time_vel <- -log(-(current_speed/max_speed -1)) * player_tau
  return(time_vel)

  }
