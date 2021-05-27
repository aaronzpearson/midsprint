
#' Combine Data
#'
#' @param distance Distance vector in the form of 10, 20, 40 yd or 9.1, 18.3, 36.6 m.
#' @param time Split times.
#' @param units Typically yards or meters.
#' @param player_name Athlete name - optional.
#'
#' @return Builds a player's 40 yard/ 36.6 meter sprint test data frame. This function feeds into combine_profile to model their speed and acceleration over time.
#'
#' @description
#'
#' combine_data and combine_profile replace optim_player_profile from the previous update, creating a more consistent coding environment amongst all _data and _profile functions.
#'
#' Please note: the distance and time vectors comprise of 3 values: time splits at 10, 20, 40 yards/ 9.1, 18.3, 36.6 meters. To visualize how the data set should look, type player_combine into the console.
#' @export
combine_data <- function(distance, time, units = "m/s", player_name = NULL) {

  UseMethod("combine_data")

}

#' @export
combine_data.default <- function(distance, time, units = "m/s", player_name = NULL) {

  player_name <- ifelse(missing(player_name),
                        round(rnorm(1, 1500, sd = 350), 0),
                        player_name)

  temp <- data.frame(distance = 0,
                     time = 0)

  temp_combine <- data.frame(distance = distance,
                             time = time)

  df <- rbind(temp, temp_combine)
  df$athlete <- player_name
  df$units <- units

  colnames(df) <- c("distance",
                    "split_times",
                    "athlete",
                    "units")

  df

}

#' Combine Profile
#'
#' @param combine_data Athlete's 40 yd/ 36.6 m data
#'
#' @return Returns a player's theoretical max speed and acceleration values. Tau values are also calculated.
#'
#' This function works in conjunction with combine_data.
#'
#' When run, the athlete's theoretical max speed and accel will be printed in the console. If saved as an object and called upon, a formatted data frame will appear. Although it is less aesthetically pleasing, all of the information remains the same.
#' @export
combine_profile <- function(combine_data) {

  UseMethod("combine_profile")

}

#' @export
combine_profile.default <- function(combine_data) {

  base_unit <- strsplit(combine_data$units[1], "/")[[1]][1]
  base_duration <- strsplit(combine_data$units[1], "/")[[1]][2]

  df <- combine_data
  df$distance = df$distance * to_meters(1, base_unit)
  df$split_times = df$split_times * to_seconds(base_duration)

  v_max0 = 10
  tau0 = 1
  x0 = c(v_max0, tau0)

  f <- function(x) {

    y = 0

    max_speed = x[1]
    player_tau = x[2]

    vel_time <- max_speed * (df$split_times + player_tau * exp(-1 * df$split_times/player_tau)) - (max_speed * player_tau)
    # Thanks to PWard for his contributions to this function

    y = sum((vel_time - df$dist)^2)

    return(y)

  }

  player <- optim(x0, f)

  max_s_metric = player$par[[1]]
  player_tau = round(player$par[[2]], 2)

  from_metric <- 1/conversion_factor(1, df$units[1])

  max_s = round(max_s_metric * from_metric, 2)
  max_a = round(max_s/player_tau, 2)


  cat(
    # returns a player's speed-accel values into the console
    paste0(
      "Athlete : ", df$athlete[1], "\n",
      "Theoretical Max Speed = ", max_s, " ", base_unit, "/", base_duration, "\n",
      "Theoretical Max Accel = ", max_a, " ",base_unit, "/", base_duration, "/", base_duration, "\n",
      "Theoretical Tau Value = ", player_tau, " s", "\n"
    )
  )


  player_profile <- data.frame(theoretical_max_speed = max_s,
                               theoretical_max_accel = max_a,
                               theoretical_tau = round(player_tau, 2),
                               distance_unit = base_unit,
                               duration_unit = base_duration,
                               player_name = combine_data$athlete[1])

}



#' @export
#' @rdname combine_data
optim_player_profile <- function(...) {

  cat("This function is no longer in use. Please use combine_data followed by combine_profile. \n
      Type ?combine_data or ?combine_profile into the console for more information.")
}
