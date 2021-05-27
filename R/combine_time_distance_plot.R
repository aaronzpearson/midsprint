#' Combine Time Distance Plot
#'
#' @param combine_profile An athlete's combine profile - required.
#' @param ... Other combine profiles - optional.
#'
#' @return Time vs distance for 1+ athletes.
#' @export
#' @rdname game_time_distance_plot
combine_time_distance_plot <- function(combine_profile, ...) {

  UseMethod("combine_time_distance_plot")

}

#' @export
combine_time_distance_plot.default <- function(combine_profile, ...) {

  combine_data_list <- list(combine_profile, ...)
  player_speed_accel_points <- do.call(rbind, lapply(combine_data_list, combine_speed_accel_values))

  p <- game_time_distance_plot_base(combine_profile) +
    annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    geom_point(data = player_speed_accel_points,
               aes(x = time_splits, y = combine_distance, colour = player_label),
               show.legend = TRUE, size = 1.5,alpha = 0.5)

  p

}

# helper functions --------------------------------------------------------

# These functions feed into combine_time_distance_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

combine_time_distance_plot_base <- function(combine_profile) {

  # similar to speed_accel_plot

  distance <- player_profile[4]
  duration <- player_profile[5]

  x_lab <- paste0("Time (", duration, ")")
  y_lab <- paste0("Distance (", distance, ")")

  ggplot() +
    theme_classic() +
    xlab(x_lab) + ylab(y_lab) +
    xlim(c(0, 5)) + ylim(c(0, 50)) +
    # please do not edit
    # packages are tough to write and compile
    # this provides the author(s) with recognition
    labs(title = "Combine Time vs. Distance",
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

combine_speed_accel_values <- function(player_profile) {

  # convert back to original metrics
  from_metric <- 1/conversion_factor(1, metric(player_profile = player_profile))

  player_prof <- player_profile
  combine_speed_accel <- player_plot_values(player_prof)
  combine_speed_accel$combine_speed <- combine_speed_accel$speed * from_metric
  combine_speed_accel$combine_accel <- combine_speed_accel$acceleration * from_metric
  combine_speed_accel$combine_distance <- combine_speed_accel$distance
  combine_speed_accel$player_label <- glue("{player_prof$player_name}
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
