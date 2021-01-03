#' Player Comparison Plot
#'
#' @param player_profile_1
#' @param player_profile_2
#'
#' @return
#' @export
#'
#' @examples
compare_player_plot <- function(player_profile_1, player_profile_2) {

  p1 <- compare_plot1(player_profile_1)

  par(mfrow = c(1,1))

  p1
  lines(speed ~ split_time, player_plot_values(player_profile_2), col = "red")
  lines(acceleration ~ split_time, player_plot_values(player_profile_2), col = "red")

  recordPlot(p1)

}
