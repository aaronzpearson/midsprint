#' Time to Reach a Distance Starting from a Given Speed
#'
#' @param player_profile Player's f-v-p profile
#' @param distance Distance to travel
#' @param current_speed The player's current speed
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#' # requires player_profile in the global environment
#' # player profile for this example: dat
#' # see player_profile function for more details
#' time_given_dist_speed(dat, 40, 3)}
#'
time_to_dist_given_speed <- function(player_profile, distance, current_speed) {

  df <- player_profile
  max_speed <- as.numeric(df[1])
  player_tau <- as.numeric(df[3])

  pos_0 <- distance_speed(df, current_speed)

  dist_dif <- time_distance(max_speed, player_tau, distance + pos_0) - time_distance(max_speed, player_tau, pos_0)
  return(dist_dif)

}
