#' Modeled Player Speed Acceleration Curves
#'
#' @param optim_player_profile
#'
#' @description
#'
#' This function returns a player's modeled speed and acceleration curves from data collected in events
#' like the NFL Combine 40 yd dash.
#'
#' @export
optim_player_plot <- function (optim_player_profile)
{
  UseMethod("optim_player_plot")
}

#' @export

optim_player_plot.default <- function (optim_player_profile) {

  par(mfrow = c(1, 1))
  optim_player_plot <- plot(speed ~ time_splits, player_plot_values(optim_player_profile),
                      type = "l", xlim = c(0, 5), ylim = c(0, 16), xlab = "Time (s)",
                      ylab = "Speed & Acceleration (yd/s & yd/s/s)",
                      main = "Projected Speed-Acceleration Plot")
  lines(acceleration ~ time_splits, player_plot_values(optim_player_profile),
        col = "red")

  legend("bottomright", c(paste("Max Speed:", round(optim_player_profile[[1]], 2)),
                          paste("Max Accel:", round(optim_player_profile[[2]], 2)),
                          paste("Tau:", round(optim_player_profile[[3]], 2))),
         col = c("black","red", "grey"), pch = 17)

  recordPlot(optim_player_plot)

}
