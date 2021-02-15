#' Player Profiling
#'
#' This function returns a player's in-situ player profile
#'
#' @param game_data
#'
#' @description
#' This function returns a data.frame containing a player's in-situ profile. The data.frame
#' includes:
#' * max observed speed
#' * max observed acceleration
#' * calculated in-situ tau value
#'
#' @return A player's unique in-situ player profile
#' @export
#'
#' @examples
#' \dontrun data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' player_a_profile <- player_profile(athlete_a)
#' player_a_profile#'
game_profile <- function (game_data)
{
  UseMethod("game_profile")
}

#' @export

game_profile.default <- function(game_data) {

  df <- game_data

  max_speed <- max(df$speed, na.rm = T)
  max_accel <- max(df$accel, na.rm = T)
  player_tau <- tau(game_data)

  player_prof <- data.frame(`Max Observed Player Speed` = max_speed,
                            `Max Observed Player Acceleration` = max_accel,
                            `Player Tau Value` = player_tau)

}
