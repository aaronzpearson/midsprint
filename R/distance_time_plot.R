#' Distance Time Plot
#'
#' @description
#' This function returns a plot detailing a player's distance covered over a given amount of time
#'
#'
#' @param player_profile player profile
#'
#' @return A player's speed and acceleration curves over time
#' @export
#'
distance_time_plot <- function (player_profile)
{
  UseMethod("distance_time_plot")
}

#' @export

distance_time_plot.default <- function(player_profile) {

  par(mfrow = c(1,1))

  player_plot <- plot(distance ~ time_splits, player_plot_values(player_profile), type = "l",
                      xlim = c(0, 5),
                      ylim = c(0, 50),
                      xlab = "Time (s)",
                      ylab = "Distance (yd)",
                      main = "Distance-Time Plot")

  recordPlot(player_plot)

}

