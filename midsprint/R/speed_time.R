#' Speed over time
#'
#' @param max_speed Max observed speed of the player
#' @param player_tau A unique tau value for each player
#' @param split_time Time
#'
#' @return Speed the player reaches at a given time from zero velocity
#'
speed_time <- function(max_speed, player_tau, split_time) {

  vel_time <- max_speed * (1 - exp(- split_time/player_tau))
  return(vel_time)

}


