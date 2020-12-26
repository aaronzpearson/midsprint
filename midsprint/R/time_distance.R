#' Time Distance
#'
#' @param max_speed Max observed speed of the player
#' @param player_tau A unique tau value for each player
#' @param time Time
#'
#' @return How long it takes to reach a given distance from zero velocity
#'
time_distance <- function(max_speed, player_tau, distance) {


  time_pos <- player_tau * lamW::lambertW0(-exp(-1 - distance/(player_tau * max_speed))) + distance/max_speed + player_tau
  return(time_pos)

}
