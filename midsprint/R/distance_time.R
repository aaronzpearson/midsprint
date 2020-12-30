#' Distance Over Time
#'
#' @param player_profile player profile from player_profile function
#' @param split_time Time
#'
#' @return The distance traveled over a given amount of time from zero velocity
#'
distance_time <- function(player_profile, split_time) {
  
  max_speed <- player_profile[1]
  player_tau <- player_profile[3]

  pos_time <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - max_speed * player_tau
  return(pos_time)

}
