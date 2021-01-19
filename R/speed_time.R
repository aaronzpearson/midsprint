#' Speed over time
#'
#' @param player_profile player profile from player_profile function
#' @param split_time Time
#'
#' @return Speed the player reaches at a given time from zero velocity
#'
speed_time <- function(player_profile, split_time) {

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  vel_time <- max_speed * (1 - exp(- split_time/player_tau))
  return(vel_time)

  }


