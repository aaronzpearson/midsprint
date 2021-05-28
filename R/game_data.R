
#' Game Data
#'
#' @param speed In-game athlete speed vector.
#' @param acceleration In-game athlete acceleration vector.
#' @param units Units used by the tracking system.
#' @param player_name Athlete name - if not provided, an arbitrary player number is assigned.
#'
#' @return Athlete game data for modeling.
#'
#' @description
#'
#' This function simplifies large data sets that are provided by positional tracking companies to four variables: speed, acceleration, units, and athlete name. This allows the {midsprint} package to model player profiles while maintaining a small footprint on the computer's memory.
#'
#' Please note that the units input are those associated with the athlete's speed. If speed and acceleration are recorded in different units, please convert to a consistent format before using this function. You can do this using the conversion_factor function to calculate the conversion constant.
#' @export
game_data <- function(speed, acceleration, units = "m/s", player_name = NULL)
{
  UseMethod("game_data")
}

#' @export
game_data.default <- function(speed, acceleration, units = "m/s", player_name = NULL) {


  player_name <- ifelse(missing(player_name),
                        round(rnorm(1, 1500, sd = 350), 0),
                        player_name)

  df <- data.frame(game_speed = speed,
                   game_accel = acceleration,
                   athlete = player_name,
                   units = units)
  df$units <- as.character(df$units)

  colnames(df) <- c("game_speed",
                    "game_accel",
                    "athlete",
                    "units")

  df

}

#' @export
#' @rdname game_data
player_data <- function(speed, acceleration, units = "m/s", player_name = NULL) {

  print("This function is no longer in use. Please use game_data()")

}

#' Game Profile
#'
#' @param game_data An athlete's game data.
#'
#' @return The athlete's game profile.
#'
#' @description
#'
#' This function is utilized in conjunction with game_data to build an athlete's in-game profile. When run, player information will be printed in the console. When recalled as an object, information is presented as a data.frame with less formatting.
#'
#' It is not necessary to run this function when modeling speed-acceleration/ acceleration-speed profiles.
#' @export
game_profile <- function(game_data) {

  UseMethod("game_profile")

}

#' @export
game_profile.default <- function(game_data) {

  df <- game_data
  max_speed <- max(df$game_speed, na.rm = T)
  max_accel <- max(df$game_accel, na.rm = T)
  player_tau <- tau(game_data)

  base_unit <- strsplit(df$units[1], "/")[[1]][1]
  base_duration <- strsplit(df$units[1], "/")[[1]][2]

  cat(
    paste0(
      "Athlete: ", game_data$athlete[1], "\n",
      "Max Observed Player Speed = ", round(max_speed, 2), " ", base_unit, "/", base_duration,
      "\n","Max Observed Player Acceleration = ",
      round(max_accel, 2), " ", base_unit, "/", base_duration,"/", base_duration, "\n",
      "Player Tau Value = ", round(player_tau, 2), " ", base_duration)
  )

  player_prof <- data.frame(max_observed_speed = max_speed,
                            max_observed_accel = max_accel,
                            game_tau = player_tau,
                            distance_unit = base_unit,
                            duration_unit = base_duration,
                            player_name = game_data$athlete[1])
}

#' @export
#' @rdname game_profile
player_profile <- function(game_data) {

  print("This function is no longer in use. Please use game_profile()")

}

# helper functions --------------------------------------------------------

tau <- function(game_data) {

  df <- game_data
  max_s <- max(df$game_speed, na.rm = T)
  max_a <- max(df$game_accel, na.rm = T)

  player_tau <- max_s/max_a
  round(player_tau, 2)

}
