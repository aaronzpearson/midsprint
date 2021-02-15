#' Time Distance
#'
#' @param player_profile player profile
#' @param time duration of time
#'
#' @return How long it takes to reach a given distance from zero velocity
#'
#' @noRd
time_distance <- function (player_profile, distance)
{
  UseMethod("time_distance")
}


time_distance.default <- function(player_profile, distance) {

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  time_pos <- player_tau * lamW::lambertW0(-exp(-1 - distance/(player_tau * max_speed))) + distance/max_speed + player_tau
  return(time_pos)

}
