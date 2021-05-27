#' Player Plot Values
#'
#' @param player_profile A data.frame built using the game_profile or combine_profile functions.
#'
#' @return Speed, acceleration, and distance values in relation to speed.
#'
#' @description
#'
#' The player_profile parameter is the player's game or optimal profile. These values are taken from in-game or 40 yd dash data and modeled to represent an athlete's sprint mechanics.
#'
#' Formulas are based off works by Morin, Samozino, Clark, etc.
#' This function is not exported.
player_plot_values <- function(player_profile) {

  # returns plot values for in-game and modeled speed, accel, dist vs time
  # create time points for f(x)
  splits <- seq(0, 5, by = 0.01)

  # build data frame
  s <- as.vector(unlist(sapply(splits, function(x) speed_time(player_profile, x))))
  a <- as.vector(unlist(sapply(splits, function(x) accel_time(player_profile, x))))
  d <- as.vector(unlist(sapply(splits, function(x) distance_time(player_profile, x))))

  # returns data frame
  # values are in metric
  data.frame(time_splits = splits,
             speed = s,
             acceleration = a,
             distance = d)

}

# helper functions --------------------------------------------------------

# These functions feed into player_plot_values() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

# These functions are utilized within game and speed accel models

speed_time <- function (player_profile, split_time) {

  # convert to metric
  to_metric <- conversion_factor(1, metric(player_profile))

  # returns speed vs time
  max_speed <- player_profile[[1]] * to_metric
  player_tau <- player_profile[[3]]
  vel_time <- max_speed * (1 - exp(-split_time/player_tau))
  return(vel_time)

}

accel_time <- function (player_profile, split_time) {

  # convert to metric
  to_metric <- conversion_factor(1, metric(player_profile))

  # returns accel vs time
  max_speed <- player_profile[[1]] * to_metric
  player_tau <- player_profile[[3]]
  acc_time <- (max_speed/player_tau) * exp(-split_time/player_tau)
  return(acc_time)
}

distance_time <- function (player_profile, split_time) {

  # convert to metric
  to_metric <- conversion_factor(1, metric(player_profile))

  # returns distance vs time
  max_speed <- player_profile[[1]] * to_metric
  player_tau <- player_profile[[3]]
  pos_time <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) -
    max_speed * player_tau
  return(pos_time)

}
