#' Title
#'
#' @param player_1_profile
#' @param player_2_profile
#' @param dist_of_player_1
#'
#' @return
#' @export
#'
#' @examples
separation_distance <- function (player_1_profile, player_2_profile, distance_to_travel)
{
  UseMethod("separation_distance")
}

#' @export

separation_distance.default <- function(player_1_profile, player_2_profile, distance_to_travel) {

  time_to_pos <- time_distance(player_1_profile, distance_to_travel)
  dist_of_player_2 <- distance_time(player_2_profile, time_to_pos)

  separation <- distance_to_travel - dist_of_player_2

  start <- "Player 1 is"
  middle <- ifelse(separation < 0, "trailing", "ahead of")
  end <- "Player 2 by"
  dist_units <- "yards."

  paste(start, middle, end, round(abs(separation), 1), dist_units)


}
