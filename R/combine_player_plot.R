
#' Combine Player Plot
#'
#' @param game_profile An athlete's combine profile - required.
#' @param ... Other combine profiles - optional.
#'
#' @return Time vs speed and acceleration for 1+ athletes.
#'
#' @description
#'
#' This function returns two curves per athlete: time versus speed and time versus acceleration. The curve moving downward is time versus acceleration, and the curve moving upward is time versus acceleration.
#'
#' This function replaces previously published functions optim_player_plot and compare_profile_plot. This function also has much simpler syntax, requiring only one argument with an indefinite number of player profiles possible.
#' @export
combine_player_plot <- function(combine_profile, ...) {

  UseMethod("combine_player_plot")

}

#' @export
combine_player_plot.default <- function(combine_profile, ...) {

  combine_data_list <- list(combine_profile, ...)
  player_combine_points <- do.call(rbind, lapply(combine_data_list, combine_speed_accel_values))

  p <- combine_plot_base(combine_profile, player_combine_points) +
    ggplot2::annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    ggplot2::geom_point(data = player_combine_points,
                        ggplot2::aes(x = time_splits, y = combine_accel, colour = player_label),
               show.legend = TRUE, size = 1.5,alpha = 0.5) +
    ggplot2::geom_point(data = player_combine_points,
                        ggplot2::aes(x = time_splits, y = combine_speed, colour = player_label),
               show.legend = FALSE, size = 1.5, alpha = 0.5)

  p

}

#' @export
#' @rdname combine_player_plot
optim_player_plot <- function(combine_profile, ...) {

  # an alternative call for game_player_plot
  combine_player_plot(combine_profile, ...)

}


# helper functions --------------------------------------------------------

# These functions feed into game_player_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

combine_speed_accel_values <- function(player_profile) {

  # convert back to original metrics
  from_metric <- 1/conversion_factor(1, metric(player_profile = player_profile))

  player_prof <- player_profile
  combine_speed_accel <- player_plot_values(player_prof)
  combine_speed_accel$combine_speed <- combine_speed_accel$speed * from_metric
  combine_speed_accel$combine_accel <- combine_speed_accel$acceleration * from_metric
  combine_speed_accel$combine_distance <- combine_speed_accel$distance
  combine_speed_accel$player_label <- glue::glue("{player_prof$player_name}
       Theoretical Max Speed = {player_prof$theoretical_max_speed }
       Theoretical Max Accel = {player_prof$theoretical_max_accel }  ")

  # returns a df in the form of:
  # df = data.frame(time_splits = 0:5s,
  # speed = speed_in_metric,
  # acceleration = acceleration_in_metric,
  # distance = distance_in_metric,
  # speed/accel/distance in original units,
  # player_label)
  combine_speed_accel

}

combine_plot_base <- function(player_profile, player_combine_points) {

  # similar to speed_accel_plot

  distance <- player_profile[4]
  duration <- player_profile[5]

  y_max <- max(player_combine_points$combine_accel, na.rm = T) + 1

  x_lab <- paste0("Time (", duration, ")")

  ggplot2::ggplot() +
    ggplot2::theme_classic() +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(paste0("Speed (", distance, "/", duration, ")",
                              "\nAcceleration (", distance, "/", duration, "/", duration, ")")) +
    ggplot2::xlim(c(0, 5)) +
    ggplot2::ylim(c(0, y_max)) +
    # please do not edit
    # packages are tough to write and compile
    # this provides the author(s) with recognition
    ggplot2::labs(title = "40 yd/ 36.6 m Speed and Acceleration Curves",
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
