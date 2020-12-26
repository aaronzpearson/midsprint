#' Distance Over Time
#'
#' @param max_speed Max observed speed of the player
#' @param player_tau A unique tau value for each player
#' @param split_time Time
#'
#' @return The distance traveled over a given amount of time from zero velocity
#'
distance_time <- function(max_speed, player_tau, split_time) {

  pos_time <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - max_speed * player_tau
  return(pos_time)

}
