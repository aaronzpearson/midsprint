
#' Game Time Distance Plot
#'
#' @param game_profile An athlete's game profile - required.
#' @param ... Other game profiles - optional.
#'
#' @return Time vs distance for 1+ athletes.
#'
#' @description
#'
#' This plot should accompany game_player_plot or a similar function to provide context.
#'
#' All values are returned in the original units found in the athlete's profile.
#' @export
game_time_distance_plot <- function(game_profile, ...) {

  UseMethod("game_time_distance_plot")

}

#' @export
game_time_distance_plot.default <- function(game_profile, ...) {

  game_data_list <- list(game_profile, ...)
  player_speed_accel_points <- do.call(rbind, lapply(game_data_list, game_speed_accel_values))

  p <- game_time_distance_plot_base(game_profile) +
    annotate("text", x = 1, y = 1,
             label = "Built with {midsprint} by @aaronzpearson",
             colour = "white") +
    geom_point(data = player_speed_accel_points,
               aes(x = time_splits, y = game_distance, colour = player_label),
               show.legend = TRUE, size = 1.5,alpha = 0.5)

  p

}


# helper functions --------------------------------------------------------

# These functions feed into game_player_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment


game_time_distance_plot_base <- function(player_profile) {

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
    labs(title = "In-Game Time vs. Distance",
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
