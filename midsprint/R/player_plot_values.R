#' Player Plot Values
#'
#' @param player_profile Must run player_profile function before player_plot_value function
#'
#' @return Plot values for f-v-p profile
#'
#' @example
#' \dontrun { tracking_data <- data.frame(speed = rnorm(100, 4, 1),
#' acceleration = rnorm(100, 2, 0.2))
#' dat <- profile_data(tracking_data, "speed", "acceleration")
#' player_data <- player_profile(dat)
#' head(player_data)
#' player_plot_values(player_data)
#' }
#'
#'
player_plot_values <- function(player_profile) {

  df <- player_profile

  max_speed <- as.numeric(df[1])
  player_tau <- as.numeric(df[3])


  profile_plot_values <- data.frame(split_time = seq(0, 5, by = 0.01))
  profile_plot_values$speed <- speed_time(max_speed, player_tau, profile_plot_values$split_time)
  profile_plot_values$acceleration <- accel_time(max_speed, player_tau, profile_plot_values$split_time)
  profile_plot_values$distance <- distance_time(max_speed, player_tau, profile_plot_values$split_time)

  return(profile_plot_values)

}
