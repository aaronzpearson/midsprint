#' Speed and Acceleration Plots over Time
#'
#' @description
#' This function returns a player's in-situ speed and acceleration v time curves
#'
#'
#' @param player_profile player profile
#'
#' @return A player's speed and acceleration curves over time
#' @export
#'
#' @examples
#' data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' player_a_profile <- player_profile(player_a_data)#'
#' player_a_speed_accel_curves <- player_plot(player_profile)}
game_player_plot <- function (player_profile)
{
  UseMethod("game_player_plot")
}

#' @export

game_player_plot.default <- function(player_profile) {

  par(mfrow = c(1,1))

  player_plot <- plot(speed ~ time_splits, player_plot_values(player_profile), type = "l",
                      xlim = c(0, 5),
                      ylim = c(0, 16),
                      xlab = "Time (s)",
                      ylab = "Speed & Acceleration (yd/s & yd/s/s)",
                      main = "Speed-Acceleration Plot")
  lines(acceleration ~ time_splits, player_plot_values(player_profile), col = "red")

  legend("bottomright",
         c(paste("Max Speed:", player_profile[[1]]),
           paste("Max Accel:", player_profile[[2]]),
           paste("Tau:", round(player_profile[[3]], 2))),
         col = c("black", "red", "grey"), pch = 17)

  recordPlot(player_plot)

}

