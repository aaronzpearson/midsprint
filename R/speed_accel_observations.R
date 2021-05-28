
#' Speed-Acceleration Model Observations
#'
#' @param game_data An athlete's game data.
#'
#' @return Observations withheld from the game data to build the speed-acceleration model.
#'
#' @description
#'
#' This function returns the observations that were utilized to build an athlete's speed-acceleration profile. As suggested by Morin and colleagues, the top two acceleration values per speed bin, cut every 0.2 m/s equal to, or above 3 m/s, are maintained. A linear model is then built and points are removed until an r squared value of 0.95 or greater is reached.
#'
#' You can also call upon accel_speed_observations for the same output.
#'
#' WARNING: This function returns the values that make up the final linear model. No intermediary values, the initially binned observations, are returned.
#'
#' For more information: https://www.researchgate.net/publication/351607405_Individual_acceleration-speed_profile_in-situ_A_proof_of_concept_in_professional_football_players
#' @export
speed_accel_observations <- function(game_data) {

  UseMethod("speed_accel_observations")

}

#' @export
speed_accel_observations.default <- function(game_data) {

  from_metric <- 1/conversion_factor(1, game_data$units[1])

  df_temp <- sa_metric_observations(game_data)

  # max accel per velocity bin
  # returns all speed-accel values in the original units

  # in the form of:
  # df = df(speed = speed_in_original_units,
  #         accel = accel_in_original units)
  df <- data.frame(speed = df_temp$speed * from_metric,
                   accel = df_temp$accel * from_metric)

  df

}

#' @export
#' @rdname speed_accel_observations
accel_speed_observations <- function(game_data) {

  # alternative function call to speed_accel_observation
  speed_accel_observations(game_data)

}

# helper functions --------------------------------------------------------

# These functions feed into speed_accel_observations() and other functions and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

sa_metric_df <- function (game_data) {

  to_metric <- conversion_factor(1, game_data$units[1])

  df <- game_data
  # converts original units to metric
  df$metric_speed = df$game_speed * to_metric
  df$metric_accel = df$game_accel * to_metric

  # rebuild a data frame in metric
  player_temp <- data.frame(speed = df$metric_speed,
                            accel = df$metric_accel)

  # filter all speed values >= 3 m/s
  player_temp <- player_temp[player_temp[, "speed"] >= 3, ]
  # bin speed values per 0.2 m/s
  player_temp$vel_bins <- cut(player_temp$speed, seq(3,
                                                     max(df$metric_speed, na.rm = T), by = 0.2))
  # set max accel in ascending order per bin
  player_temp <- player_temp[order(player_temp$accel), ]

  # select top two accel values in each bin
  temp <- by(player_temp, player_temp["vel_bins"], tail, 2)
  # bind all values into a data frame
  player_temp <- Reduce(rbind, temp)

  # max accel per velocity bin
  # returns all speed-accel values in the original metrics

  # returns a df in the form of:
  # df = df(speed = speed_in_metric,
  #         accel = accel_in_metric)
  player_temp

}

sa_metric_observations <- function(game_data) {

  # feeds into speed_accel_observations
  df_temp <- sa_metric_df(game_data)

  fit <- lm(accel ~ speed, data = df_temp)
  r_square <- summary(fit)[[8]]
  y_int <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  x_int <- y_int/abs(slope)

  while (r_square < 0.95) {
    df_temp$fit_predict <- y_int + (slope * df_temp$speed)
    df_temp$diff_predict <- abs(df_temp$accel - df_temp$fit_predict)
    df_temp <- df_temp[order(df_temp$diff_predict), ]
    df_temp <- head(df_temp, nrow(df_temp) - 1)
    fit <- lm(accel ~ speed, data = df_temp)
    r_square <- summary(fit)[[8]]
    y_int <- coef(fit)[[1]]
    slope <- coef(fit)[[2]]
    x_int <- y_int/abs(slope)
  }

  # returns a df in metric
  # all returned values are those remaining in the model fit
  # typically 30-50 remaining observations

  # in the form of:
  # df = df(speed = speed_in_metric,
  #         accel = accel_in_metric)
  df_temp


}

sa_plot_observations <- function(game_data) {

  # returns a df in metric
  df_temp <- sa_metric_observations(game_data)

  # binds player speed-accel profile to the data
  # creates a messy df but is not seen on the front-end
  # sa_muted returns values in the original units
  df <- cbind(df_temp, sa_muted(game_data))

  from_metric <- 1/ conversion_factor(1, game_data$units[1])

  # rebuild speed-accel observations in the original units
  df$game_speed <- df$speed * from_metric
  df$game_accel <- df$accel * from_metric
  # creates player labels
  # used for player colour in the plot
  # glue::glue returns a multi-line string for cleaner labeling
  df$player_label <-   glue::glue("{df$player}
       Max Speed = {df$theoretical_max_speed}
       Max Accel = {df$theoretical_max_accel}  ")

  # in the form of:
  # df(speed = speed_in_metric,
  #    accel = accel_in_metric,
  #    game_speed =speed_in_original_unit,
  #    game_accel = accel_in_original_inits,
  #    ...)
  #    ... = sa_muted values and label built off them

  # plot aes: aes(x = game_speed, y = game_accel, ...)
  return(df)

}
