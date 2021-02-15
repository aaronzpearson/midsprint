#' Distance Time Comparison Plot
#'
#'@description
#'This function returns a plot comparing two player profiles. These profiles can originate from
#'two players or can compare a player's modeled and in-situ profiles.
#'
#' @param player_profile_1 player 1 profile
#' @param player_profile_2 player 2 profile
#'
#' @return
#' @export
#'
#' @examples
#' data(player_a)
#' data(player_b)
#' player_a_data <- tracking_data(player_a)
#' player_a_profile <- player_profile(player_a_data)
#'
#' player_b_data <- tracking_data(player_b)
#' player_b_profile <- player_profile(player_b)
#'
#' player_a_b_compare_plot <- compare_player_plot(player_a_profile, player_b_profile)

compare_player_distance_time_plot <- function (player_profile_1, player_profile_2)
{
  UseMethod("compare_player_distance_time_plot")
}

#' @export

compare_player_distance_time_plot.default <- function(player_profile_1, player_profile_2) {

  p1 <- compare_dist_time_plot1(player_profile_1)

  par(mfrow = c(1,1))

  p1
  lines(distance ~ time_splits, player_plot_values(player_profile_2), col = "red")

  recordPlot(p1)

}
