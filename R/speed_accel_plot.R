
#' Speed-Acceleration Plot
#'
#' @param game_data An athlete's game data - required.
#' @param ... Other game data - optional.
#'
#' @return A speed-acceleration plot for 1+ athletes.
#'
#' @description
#' This function returns an athlete's speed-acceleration plot.
#'
#' When a single athlete's game_data is provided, the plot returns a plot with three layers: 1. a sample of 5000 observations from their game data, 2. points that are demarcated in a different color, indicating that they were utilized in building the linear model, and 3. the linear model itself in the form of a line of least squares. Multiple athletes on a plot will only return their linear models, in the form of a line of least squares.
#'
#' An athlete's information is placed at the bottom of the graph. Information in the plot legend includes the athlete's name, their theoretical max speed, and their theoretical max accel. Although this might not be aesthetically pleasing for single player plots, it is efficient for multiple players. It is also cleaner when this graph is included in player reports.
#'
#' You can also call on accel_speed_plot to render the same plot.
#'
#' WARNING: All values returned will be in the original units provided in the game_data function. If players have different units associated to their game_data, the units from the first athlete on the list will be utilized.
#' @export
speed_accel_plot <- function(game_data, ...) {

 UseMethod("speed_accel_plot")

  }

#' @export
speed_accel_plot.default <- function(game_data, ...) {

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
  player_speed_accel_points <- do.call(rbind, lapply(game_data_list, sa_plot_observations))

  # number of players in the plot
  n_games <- length(game_data_list)

  # plotting aesthetics
  max_s <- conversion_factor(max(player_speed_accel_points$theoretical_max_speed),
                             units = game_data$units[1]) + 1
  max_a <- conversion_factor(max(player_speed_accel_points$theoretical_max_accel),
                             units = game_data$units[1]) + 1

  # if else for 1 or 2+ players
  if(n_games == 1) {

    # take sample 5k points from data set
    # decreases rendering time
    game_data_sample <- game_data[sample(1:nrow(game_data), 5000, replace = T),]

    # build single player plot
    p <- sa_plot_base(game_data, max_s, max_a) +
      ggplot2::annotate("text", x = 1, y = 1,
               label = "Built with {midsprint} by @aaronzpearson",
               colour = "white") +
      # points from asample data
      ggplot2::geom_point(data = game_data_sample,
                          ggplot2::aes(x = game_speed, y = game_accel),
                 colour = "grey", alpha = 0.3) +
      # points from sa_plot_observations
      # returned in  original units
      ggplot2::geom_point(data = player_speed_accel_points,
                          ggplot2::aes(x = game_speed, y = game_accel, colour = player_label),
                 size = 2, show.legend = TRUE) +
      # linear fit to the sa_plot_observations
      ggplot2::geom_smooth(data = player_speed_accel_points,
                           ggplot2::aes(x = game_speed, y = game_accel, colour = player_label),
                  se = F, method = "lm", fullrange = T,
                  show.legend = FALSE, size = 1.5, alpha = 0.8)

    } else {

    # build multi-player plot
    p <- sa_plot_base(game_data, max_s, max_a) +
      ggplot2::annotate("text", x = 1, y = 1,
               label = "Built with {midsprint} by @aaronzpearson",
               colour = "white") +
      # linear fit to all players in the data set
      ggplot2::geom_smooth(data = player_speed_accel_points,
                           ggplot2::aes(x = game_speed, y = game_accel, colour = player_label),
                  se = F, method = "lm", fullrange = T,
                  show.legend = TRUE, size = 2)


  }

  # print to 'plot'
  # suppresses messages and warning from ggplot for cleaner output in the console
  # limits error traceback and trouble-shooting
    # unintentionally done
  p

  }

#' @export
#' @rdname speed_accel_plot
accel_speed_plot <- function(game_data, ...) {

  # alternative call to speed_accel_plot
  speed_accel_plot(game_data, ...)

  }

# helper functions --------------------------------------------------------

# These functions feed into speed_accel_plot() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

sa_plot_base <- function(game_data, max_s = max_s, max_a = max_a) {

  # helper to label objects
  units <- game_data$units[1]
  distance <- strsplit(units, "/")[[1]][1]
  duration <- strsplit(units, "/")[[1]][2]

  # labels
  y_lab <- paste0("Acceleration (", distance,"/", duration, "/", duration, ")")
  x_lab <- paste0("Speed (", distance,"/", duration, ")")

  # build aesthetics of speed-accel plots
  ggplot2::ggplot() +
    ggplot2::theme_classic() +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, (max_s + 1)), breaks = seq(0, max_s, by = 2)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, (max_a + 1)), breaks = seq(0, max_s, by = 2)) +
    # please do not edit
    # packages are tough to write and compile
    # this provides the author(s) with recognition
    ggplot2::labs(title = "In-Game Speed-Acceleration Model",
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
