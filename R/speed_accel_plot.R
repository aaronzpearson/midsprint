#' In-Situ Speed-Acceleration Plot
#'
#' This model returns a player's in-situ speed-acceleration plot
#'
#' @param game_data
#'
#' @description
#' The plot returned includes the following components:
#' * all data player observations (grey)
#' * final observations modeled (red)
#' * regression line (black line)
#' * legend containing player details (modeled max speed, acceleration, and tau)
#'
#' @return In-situ speed-accel player profile plot
#' @export
#'
#' @examples
#' data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' player_a_insitu_plot <- speed_accel_plot(player_a_data)
#' player_a_insitu_plot
speed_accel_plot <- function (game_data)
{
  UseMethod("speed_accel_plot")
}

#' @export

speed_accel_plot.default <- function(game_data) {

  player_values <- speed_accel_values(game_data)
  colnames(player_values) <- c("speed", "accel", "vel_bins")
  model_df <- data.frame(accel = c(0, speed_accel(game_data)[[1]]),
                         speed = c(speed_accel(game_data)[[2]], 0))
  game_data_short <- game_data[sample(1:nrow(game_data), 5000, replace = T),]

  speed_accel_p <- plot(accel ~ speed, data = game_data_short,
                        col = "grey", pch = 20,
                        xlim = c(0, model_df[1,2] + 1),
                        ylim = c(0, model_df[2,1] + 1),
                        xaxs = "i",
                        yaxs = "i",
                        ylab = "Acceleration (yd/s/s)",
                        xlab = "Speed (yd/s)",
                        main = "Player Speed-Acceleration Profile")
  lines(accel ~ speed, data = model_df)
  points(df.accel ~ df.speed, speed_accel_points(game_data),
         col = "black")
  points(df.accel ~ df.speed, speed_accel_points(game_data),
         col = "red", pch = 20)
  legend("topright", c(paste("Projected Max Speed:", round(model_df[1,2], 2)),
                          paste("Projected Max Accel:", round(model_df[2,1], 2)),
                          paste("Tau:", round(model_df[1,2]/ model_df[2,1], 2))),
         col = c("black","red", "grey"), pch = 17)

  recordPlot(speed_accel_p)

  }
