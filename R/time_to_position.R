#' Time to Position
#'
#' @param player_profile player profile
#' @param current_speed current speed
#' @param distance distance to travel
#'
#' @return The amount of time it takes to reach a given position having velocity >= 0
#'
#' @noRd
time_to_position <- function (player_profile, current_speed, distance)
{
  UseMethod("time_to_position")
}

time_to_position.default <- function(player_profile, current_speed, distance) {

  time_to_dist <- time_to_dist_given_speed(player_profile, distance, current_speed)
  round(time_to_dist, 2)


}
