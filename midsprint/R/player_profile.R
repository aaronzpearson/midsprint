#' Player Profile
#'
#' @param profile_data Must run profile_data function before player_profile function
#'
#' @return A player's unique profile
#' @export
#'
#' @examples
#' \dontrun {tracking_data <- data.frame(speed = rnorm(100, 4, 1),,
#' acceleration = rnorm(100, 2, 0.2))
#' dat <- profile_data(tracking_data, "speed", "acceleration")
#' head(dat)
#' player <- player_profile(dat)
#' print(player)}
#'
player_profile <- function(profile_data) {

  df <- profile_data

  max_speed <- max(df$speed, na.rm = T)
  max_accel <- max(df$accel, na.rm = T)
  player_tau <- tau(profile_data)

  player_prof <- data.frame(`Max Observed Player Speed` = max_speed,
                            `Max Observed Player Acceleration` = max_accel,
                            `Player Tau Value` = player_tau)

}
