
#' Speed-Acceleration Model
#'
#' @param game_data An athlete's game data.
#'
#' @return An athlete's theoretical max speed and max accel.
#'
#' @description
#'
#' This function returns an athlete's theoretical maximal speed and acceleration from their game data, called using the game_data function.
#' When called upon, you will see the athlete's information displayed in the console. When you save the function to an object, it will return a data.frame with the player's information. The data.frame is less formatted than the original call but contains the same information.
#'
#' The model relies on multiple helper functions. These functions convert the original game data to metric, and the linear model is fit using a least squares regression. The model is fit to a minimum r squared value of 0.95.
#'
#' The speed-acceleration function is modelled based on Morin and colleagues work: https://www.researchgate.net/publication/351607405_Individual_acceleration-speed_profile_in-situ_A_proof_of_concept_in_professional_football_players
#'
#' You can also call upon accel_speed for the same output.
#' @export
speed_accel <- function(game_data) {

  UseMethod("speed_accel")

  }

#' @export
speed_accel.default <- function(game_data) {

  # sa_muted return speed-accel values, no output
  df <- sa_muted(game_data)

  base_unit <- strsplit(game_data$units[1], "/")[[1]][1]
  base_duration <- strsplit(game_data$units[1], "/")[[1]][2]
  # converts metric values to original outputs
  from_metric <- conversion_factor(1, game_data$units[1])

  cat(
    # returns a player's speed-accel values into the console
    paste0(
      "Athlete : ", game_data$athlete[1], "\n",
      "Theoretical Max Speed = ", df[[1]], " ", base_unit, "/", base_duration, "\n",
      "Theoretical Max Accel = ", df[[2]], " ",base_unit, "/", base_duration, "/", base_duration, "\n",
      "Theoretical Tau Value = ", df[[3]], " s", "\n",
      "r Squared = ", df[[5]], "\n",
      "Number of Observations = ", df[[6]]
    )
  )

  # saves speed-accel values for future recall
  # does not call console output
  player_values <- df

  }

#' @export
#' @rdname speed_accel
accel_speed <- function(game_data) {

  # alternative function call to speed_accel
  speed_accel(game_data)

  }

# helper functions --------------------------------------------------------

# These functions feed into speed_accel() and other functions and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

sa_muted <- function(game_data) {
  # returns speed-accel values without a a printout to the console

  df_temp <- sa_metric_observations(game_data)
  # initial linear model
  fit <- lm(accel ~ speed, data = df_temp)
  r_square <- summary(fit)[[8]]
  y_int <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  x_int <- y_int/abs(slope)

  # while-loop fits the speed-accel model
  # continues until r_squared value is greater than, or equal to 0.95
  while (r_square <= 0.95) {
    df_temp$fit_predict <- y_int + (slope * df_temp$speed)
    df_temp$diff_predict <- abs(df_temp$accel - df_temp$fit_predict)
    df_temp <- df_temp[order(df_temp$diff_predict), ]
    # returns df with one less row per loop until loop completes
    df_temp <- head(df_temp, nrow(df_temp) - 1)
    fit <- lm(accel ~ speed, data = df_temp)
    r_square <- summary(fit)[[8]]
    y_int <- coef(fit)[[1]]
    slope <- coef(fit)[[2]]
    x_int <- y_int/abs(slope)
  }

  # fmax
  y_int <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  # vmax
  x_int <- y_int/abs(slope)

  from_metric <- conversion_factor(1, game_data$units[1])

  # returns speed-accel values in the original units
  player_values <- data.frame(theoretical_max_speed = round(x_int/ from_metric, 1),
                              theoretical_max_accel = round(y_int/ from_metric, 1),
                              theoretical_tau = round(x_int / y_int, 2),
                              units = game_data$units[1],
                              r_square = round(r_square, 2),
                              n_obervation = nrow(df_temp),
                              player = game_data$athlete[1])

}

