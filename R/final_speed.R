#' Final speed reached after a given distance
#'
#' @param player_profile
#' @param current_speed
#' @param distance
#'
#' @return
#' @export
#'
#' @examples
final_speed <- function (player_profile, current_speed, distance)
{
  UseMethod("final_speed")
}

#' @export

final_speed.default <- function(player_profile, current_speed, distance) {

  time_to_pos <- time_to_position(player_profile, current_speed, distance)
  time_vel <- time_speed(player_profile, current_speed)

  v_final <- speed_time(player_profile, time_to_pos + time_vel)
  round(v_final, 2)

}

