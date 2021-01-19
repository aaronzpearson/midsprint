#' Time to Reach a Distance Starting from a Given Speed
#'
#' @param player_profile Player's f-v-p profile
#' @param distance Distance to travel
#' @param current_speed The player's current speed
#'
#' @return
#'
#' @examples
time_to_dist_given_speed <- function(player_profile, distance, current_speed) {

  df <- player_profile
  max_speed <- df[[1]]
  player_tau <- df[[3]]

  pos_0 <- distance_speed(df, current_speed)

  dist_dif <- time_distance(df, distance + pos_0) - time_distance(df, pos_0)
  return(dist_dif)

}
