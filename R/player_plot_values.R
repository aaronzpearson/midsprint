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
  
  splits <- seq(0, 5, by = 0.01)
  
  s <- as.vector(unlist(sapply(splits, function(x) speed_time(player_profile, x))))
  a <- as.vector(unlist(sapply(splits, function(x) accel_time(player_profile, x))))
  d <- as.vector(unlist(sapply(splits, function(x) distance_time(player_profile, x))))
  
  data.frame(time_splits = splits,
             speed = s,
             acceleration = a,
             distance = d)
  
}