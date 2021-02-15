#' Time to Reach a Distance Starting from a Given Speed
#'
#' @param player_profile player profile
#' @param distance distance to travel
#' @param current_speed the player's current speed
#'
#' @return helper function
#'
#' @noRd
time_to_dist_given_speed <- function (player_profile, distance, current_speed)
{
  UseMethod("time_to_dist_given_speed")
}


time_to_dist_given_speed.default <- function(player_profile, distance, current_speed) {

  df <- player_profile
  max_speed <- df[[1]]
  player_tau <- df[[3]]

  pos_0 <- distance_speed(df, current_speed)

  dist_dif <- time_distance(df, distance + pos_0) - time_distance(df, pos_0)
  return(dist_dif)

}
