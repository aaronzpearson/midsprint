
#' Game Player Plot
#'
#' @param game_profile An athlete's game profile - required.
#' @param ... Other game profiles - optional.
#'
#' @return Time vs speed and acceleration for 1+ athletes.
#'
#' @description
#'
#' This function returns two curves per athlete: time versus speed and time versus acceleration. The curve moving downward is time versus acceleration, and the curve moving upward is time versus acceleration.
#'
#' This function replaces previously published functions game_player_plot and compare_profile_plot. This function also has much simpler syntax, requiring only one argument with an indefinite number of player profiles possible.
#' @export
game_player_plot <- function(game_profile, ...) {

  UseMethod("game_player_plot")

}

#' @export
game_player_plot.default <- function(game_profile, ...) {

  game_data_list <- list(game_profile, ...)
  player_speed_accel_points <- do.call(rbind, lapply(game_data_list, game_speed_accel_values))

  p <- game_speed_accel_plot_base(game_profile, player_speed_accel_points) +
    annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    geom_point(data = player_speed_accel_points,
                aes(x = time_splits, y = game_accel, colour = player_label),
                show.legend = TRUE, size = 1.5,alpha = 0.5) +
    geom_point(data = player_speed_accel_points,
               aes(x = time_splits, y = game_speed, colour = player_label),
               show.legend = FALSE, size = 1.5, alpha = 0.5)

  p

}

#' @export
#' @rdname game_player_plot
game_athlete_plot <- function(game_profile, ...) {

  # an alternative call for game_player_plot
  game_player_plot(game_profile, ...)

}


# helper functions --------------------------------------------------------

# These functions feed into game_player_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

game_speed_accel_values <- function(player_profile) {

  # convert back to original metrics
  from_metric <- 1/conversion_factor(1, metric(player_profile = player_profile))

  player_prof <- player_profile
  game_speed_accel <- player_plot_values(player_prof)
  game_speed_accel$game_speed <- game_speed_accel$speed * from_metric
  game_speed_accel$game_accel <- game_speed_accel$acceleration * from_metric
  game_speed_accel$game_distance <- game_speed_accel$distance
  game_speed_accel$player_label <- glue("{player_prof$player_name}
       Max Speed = {player_prof$max_observed_speed }
       Max Accel = {player_prof$max_observed_accel }  ")

  # returns a df in the form of:
  # df = data.frame(time_splits = 0:5s,
                    # speed = speed_in_metric,
                    # acceleration = acceleration_in_metric,
                    # distance = distance_in_metric,
                    # speed/accel/distance in original units,
                    # player_label)
  game_speed_accel

}

game_speed_accel_plot_base <- function(player_profile, player_speed_accel_points) {

  # similar to speed_accel_plot

  distance <- player_profile[4]
  duration <- player_profile[5]

  y_max <- max(player_speed_accel_points$game_accel, na.rm = T) + 1

  x_lab <- paste0("Time (", duration, ")")

  ggplot() +
    theme_classic() +
    xlab(x_lab) + ylab(paste0("Speed (", distance, "/", duration, ")",
                              "\nAcceleration (", distance, "/", duration, "/", duration, ")")) +
    xlim(c(0, 5)) + ylim(c(0, y_max)) +
    # please do not edit
    # packages are tough to write and compile
    # this provides the author(s) with recognition
    labs(title = "In-Game Speed and Acceleration Curves",
         subtitle = "Built with {midsprint} by @aaronzpearson",
         colour = "") +
    # distinct player colours
    scale_colour_brewer(palette = "Set1") +
    theme(plot.subtitle = element_text(hjust = 0.5,
                                       color = "#666666",
                                       size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}
