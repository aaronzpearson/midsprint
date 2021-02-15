#' Player Plot Values
#'
#' @param player_profile player profile
#'
#' @return Plot values for f-v-p profile
#'
#' @example
#' data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' player_a_profile <- player_profile(player_a_data)
#' dat <- profile_data(tracking_data, "speed", "acceleration")
#' player_data <- player_profile(dat)
#' head(player_data)
#' player_plot_values(player_data)
#' }
#'
#'
#' @noRd
player_plot_values <- function (player_profile)
{
  UseMethod("player_plot_values")
}


player_plot_values.default <- function(player_profile) {

  splits <- seq(0, 5, by = 0.01)

  s <- as.vector(unlist(sapply(splits, function(x) speed_time(player_profile, x))))
  a <- as.vector(unlist(sapply(splits, function(x) accel_time(player_profile, x))))
  d <- as.vector(unlist(sapply(splits, function(x) distance_time(player_profile, x))))

  data.frame(time_splits = splits,
             speed = s,
             acceleration = a,
             distance = d)

}
