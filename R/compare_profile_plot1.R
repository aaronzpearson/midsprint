#' Player Comparison Plot
#'
#' Helper function
#'
#' @param player_profile_1 player 1 profile
#'
#' @return A helper function for compare_player_plot()
#'
#' @noRd
compare_profile_plot1 <- function(player_profile_1)
{
  UseMethod("compare_profile_plot1")
}

compare_profile_plot1.default <- function (player_profile_1) {

  player_plot_values(player_profile_1)

  par(mfrow = c(1,1))

  player_plot <- plot(speed ~ time_splits, player_plot_values(player_profile_1), type = "l",
  xlim=c(0, 5), ylim = c(0, 16),
  xlab = "Time (s)",
  ylab = "Speed (yd/s)",
  main = "Speed-Acceleration Comparison Plot")
  lines(acceleration ~ time_splits, player_plot_values(player_profile_1))

  legend("bottomright",
         c("Player 1", "Player 2"),
         col = c("black", "red"), pch = 17)


  recordPlot(player_plot)

}
