
#' Distance-Time Plot
#'
#' @param game_data An athlete's game data - required.
#' @param ... Other game data - optional.
#'
#' @return A distance-time plot for 1+ athletes.
#'
#' @description
#'
#' This plot should be called with the speed_accel_plot, as part of the player_report function to provide context.
#'
#' @rdname game_distance_time_plot
#' @export
speed_accel_time_distance_plot <- function(game_data, ...) {

  UseMethod("speed_accel_time_distance_plot")

}

#' @export
speed_accel_time_distance_plot.default <- function(game_data, ...) {

  # create list of all game data
  game_data_list <- list(game_data, ...)

  # applies sa_plot_observations to each data set
  # then binds all data sets

  # in the form of:
  # df(speed = speed_in_metric,
  #    accel = accel_in_metric,
  #    game_speed = speed_in_original_unit,
  #    game_accel = accel_in_original_inits,
  #    ...)
  #    ... = sa_muted values and label built off them
  player_dist_time_points <- do.call(rbind, lapply(game_data_list, sa_dist_time_values))

  p <- sa_dist_time_plot_base(game_data) +
    ggplot2::annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    # linear fit to all players in the data set
    ggplot2::geom_point(data = player_dist_time_points,
                        ggplot2::aes(x = time_splits, y = distance, colour = player_label),
               show.legend = TRUE, size = 1.5)

  p

}

#' @export
#' @rdname game_distance_time_plot
accel_speed_distance_time_plot <- function(game_data, ...) {

  # alternative call for speed_accel_dist_time_plot
  p <- speed_accel_time_distance_plot(game_data)
  p

}

# helper functions --------------------------------------------------------

# These functions feed into speed_accel_dist_time_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

sa_dist_time_values <- function(game_data) {

  player_prof <- sa_muted(game_data)
  sa_dist_time <- player_plot_values(player_prof)
  sa_dist_time$player_label <- glue::glue("{player_prof$player}
       Max Speed = {player_prof$theoretical_max_speed}
       Max Accel = {player_prof$theoretical_max_accel}  ")

  sa_dist_time

}

sa_dist_time_plot_base <- function(game_data) {

  # helper to label objects
  distance <- strsplit(game_data$units[1], "/")[[1]][1]
  duration <- strsplit(game_data$units[1], "/")[[1]][2]

  # labels
  y_lab <- paste0("Distance (", distance, ")")
  x_lab <- paste0("Time (", duration, ")")

  # build aesthetics of speed-accel plots
  ggplot2::ggplot() +
    ggplot2::theme_classic() +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::xlim(c(0, 5)) +
    ggplot2::ylim(c(0, 50)) +
    # please do not edit
    # packages are tough to write and compile
    # this provides the author(s) with recognition
    ggplot2::labs(title = "Distance-Time Player Comparison",
         subtitle = "Built with {midsprint} by @aaronzpearson",
         colour = "") +
    # distinct player colours
    ggplot2::scale_colour_brewer(palette = "Set1") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                       color = "#666666",
                                       size = 8),
          plot.title = ggplot2::element_text(hjust = 0.5),
          legend.position = "bottom")

}

sa_dist_time_report_plot_base <- function(game_data) {

  # helper to label objects
  distance <- strsplit(game_data$units[1], "/")[[1]][1]
  duration <- strsplit(game_data$units[1], "/")[[1]][2]

  # labels
  y_lab <- paste0("Distance (", distance, ")")
  x_lab <- paste0("Time (", duration, ")")

  # build aesthetics of speed-accel plots
  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::xlim(c(0, 5)) +
    ggplot2::ylim(c(0, 50)) +
    # distinct player colours
    ggplot2::scale_colour_brewer(palette = "Set1") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                       color = "#666666",
                                       size = 8),
          plot.title = ggplot2::element_text(hjust = 0.5),
          legend.position = "bottom")

}

speed_accel_dist_time_report_plot <- function(game_data, ...) {

  # create list of all game data
  game_data_list <- list(game_data, ...)

  # applies sa_plot_observations to each data set
  # then binds all data sets

  # in the form of:
  # df(speed = speed_in_metric,
  #    accel = accel_in_metric,
  #    game_speed = speed_in_original_unit,
  #    game_accel = accel_in_original_inits,
  #    ...)
  #    ... = sa_muted values and label built off them
  player_dist_time_points <- do.call(rbind, lapply(game_data_list, sa_dist_time_values))

  p <- sa_dist_time_report_plot_base(game_data) +
    ggplot2::annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    # linear fit to all players in the data set
    ggplot2::geom_point(data = player_dist_time_points,
                        ggplot2::aes(x = time_splits, y = distance, colour = player_label),
               show.legend = FALSE, size = 1.5)

  p

}
