#' Speed-Acceleration Plot
#'
#' @param player_profile
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {p <- player_plot(player_profile)}
player_plot <- function(player_profile) {

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

