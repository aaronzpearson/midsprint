#' Time Speed
#'
#' @param max_speed Max observed speed of the player
#' @param current_speed The player's current speed
#' @param player_tau A unique tau value for each player
#'
#' @return How long it takes to reach max speed with a starting speed >= 0
#'
time_speed <- function(max_speed, current_speed, player_tau) {

  time_vel <- -log(-(current_speed/max_speed -1)) * player_tau
  return(time_vel)

  }
