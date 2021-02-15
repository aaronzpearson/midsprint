#' Player Tau
#'
#' @param profile_data Must run profile_data function before tau function
#'
#' @return A player's unique tau value
#'
#' @noRd
tau <- function (game_data)
{
  UseMethod("tau")
}


tau.default <- function(game_data) {

  df <- game_data
  max_s <- max(df$speed, na.rm = T)
  max_a <- max(df$accel, na.rm = T)

  player_tau <- max_s/max_a
  return(player_tau)

}
