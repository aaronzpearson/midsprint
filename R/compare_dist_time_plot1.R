#' Player Distance-Time Comparison Plot
#'
#' Helper function
#'
#' @param player_profile_1 player 1 profile
#'
#' @return A helper function for compare_player_plot()
#'
#' @noRd
compare_dist_time_plot1 <- function(player_profile_1)
{
  UseMethod("compare_dist_time_plot1")
}


compare_dist_time_plot1.default <- function (player_profile_1) {

  player_plot_values(player_profile_1)

  par(mfrow = c(1,1))

  player_plot <- plot(distance ~ time_splits, player_plot_values(player_profile_1), type = "l",
                      xlim=c(0, 5), ylim = c(0, 50),
                      xlab = "Time (s)",
                      ylab = "Distance (yd)",
                      main = "Distance-Time Comparison Plot")

  legend("bottomright",
         c("Player 1", "Player 2"),
         col = c("black", "red"), pch = 17)


  recordPlot(player_plot)

}
