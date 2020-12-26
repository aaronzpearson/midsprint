#' Player Tau
#'
#' @param profile_data Must run profile_data function before tau function
#'
#' @return A player's unique tau value
#'
tau <- function(profile_data) {

  df <- profile_data
  max_s <- max(df$speed, na.rm = T)
  max_a <- max(df$accel, na.rm = T)

  player_tau <- max_s/max_a
  return(player_tau)

}
