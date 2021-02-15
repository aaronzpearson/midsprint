#' Player Profile
#'
#' @param profile_data Must run profile_data function before player_profile function
#'
#' @return A player's unique profile
#'
#' @examples
#' data(player_a)
#' player_a_data <- tracking_data(player_a$speed, player_a$accel)
#' player_a_profile <- player_profile(player_a_data)
#' player_a_profile
#'
player_profile <- function (profile_data)
{
  UseMethod("player_profile")
}


player_profile.default <- function(profile_data) {

  df <- profile_data

  max_speed <- max(df$speed, na.rm = T)
  max_accel <- max(df$accel, na.rm = T)
  player_tau <- tau(profile_data)

  player_prof <- data.frame(`Max Observed Player Speed` = max_speed,
                            `Max Observed Player Acceleration` = max_accel,
                            `Player Tau Value` = player_tau)

  print("This function is depreciated. Please use game_profile() when using game data and optim_player_profile() when using Combine-like data.")

}
