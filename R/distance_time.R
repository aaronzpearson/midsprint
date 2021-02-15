#' Distance Over Time
#'
#' @description
#' This function returns the distance an athlete can travel over a distinct duration when starting with
#' velocity = 0
#'
#' @param player_profile player profile from player_profile function
#' @param split_time duration of time
#'
#' @return The distance traveled over a given amount of time from zero velocity
#'
#' @noRd
distance_time <- function (player_profile, split_time)
{
  UseMethod("distance_time")
}


distance_time <- function(player_profile, split_time) {

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  pos_time <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - max_speed * player_tau
  return(pos_time)

}
