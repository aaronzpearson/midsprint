#' Time to Position
#'
#' @param player_profile
#' @param current_speed
#' @param distance
#'
#' @return
#' @export
#'
#' @examples
time_to_position <- function(player_profile, current_speed, distance) {

  time_to_dist <- time_to_dist_given_speed(player_profile, distance, current_speed)
  round(time_to_dist, 2)


}
