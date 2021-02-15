#' Separation distance between players while in motion
#'
#' @param player_profile1
#' @param player_speed1
#' @param distance1
#' @param player_profile2
#' @param player_speed2
#' @param distance2
#'
#' @return
#' @export
#'
#' @examples
separation_in_motion <- function (player_1_profile, player_1_speed, player_1_distance,
                                 player_2_profile, player_2_speed, player_2_distance)
{
  UseMethod("separation_in_motion")
}

#' @export

separation_in_motion.default <- function(player_1_profile, player_1_speed, player_1_distance, player_2_profile, player_2_speed, player_2_distance) {

 player1_time_to_pos <- time_to_position(player_1_profile, player_1_speed, player_2_distance)

 player2_time_speed <- time_speed(player_2_profile, player_2_speed)
 player2_dist_travelled <- distance_time(player_2_profile, player1_time_to_pos + player2_time_speed) -
   distance_time(player_2_profile, player2_time_speed)

 dist_diff = player_1_distance - player2_dist_travelled + (player_1_distance - player_2_distance)

 start <- "Player A"
 middle <- ifelse(dist_diff < 0, "trails", "is ahead of")
 end <- "Player B by"
 dist_units <- "yards."

 paste(start, middle, end, round(abs(dist_diff), 1), dist_units)


}
