#' Comparing Two Players to Reach a Position
#'
#' @param player_1_profile
#' @param player_1_speed
#' @param player_1_distance
#' @param player_2_speed
#' @param player_2_distance
#' @param player_2_profile
#'
#' @return
#' @export
#'
#' @examples
compare_time_to_position <- function(player_1_profile, player_1_speed, player_1_distance, player_2_profile, player_2_speed, player_2_distance) {

  player_1_time_to_pos <- time_to_position(player_1_profile, player_1_speed, player_1_distance)
  player_2_time_to_pos <- time_to_position(player_2_profile, player_2_speed, player_2_distance)


  str1 <- "Player 1:"
  str2 <- "Player 2:"
  seconds <- "seconds."

  result1 <- paste(str1, player_1_time_to_pos, seconds)
  result2 <- paste(str2, player_2_time_to_pos, seconds)

  c(result1, result2)

}
