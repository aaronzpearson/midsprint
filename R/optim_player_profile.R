#' Modeled Player Profile
#'
#' @description
#'
#' This function returns a player profile from events like the NFL Combine 40 yd dash
#'
#' @param distance distance traveled
#' @param splits split times
#'
#' @return Optimized player profile
#' @export
#'
#' @examples
#' player_a_model_profile <- model_player_profile(combine_player_a$dist, combine_player_a$split)
#' player_a_model_profile
optim_player_profile <- function (distance, splits)
{
  UseMethod("optim_player_profile")
}

#' @export

optim_player_profile.default <- function(distance, splits) {

  dist <- distance
  split <- splits

  df <- data.frame(dist, split)
  colnames(df) <- c("dist", "split")

  v_max0 = 10
  tau0 = 1
  x0 = c(v_max0, tau0)

  f <- function(x) {

    y = 0

    max_speed = x[1]
    player_tau = x[2]

    vel_time <- max_speed * (df$split + player_tau * exp(-1 * df$split/player_tau)) - (max_speed * player_tau)
    # Thanks to PWard for his contributions to this function

    y = sum((vel_time - df$dist)^2)

    return(y)

  }

  player <- optim(x0, f)

  max_s = player$par[[1]]
  player_tau = player$par[[2]]


  player_profile <- data.frame(`Max Modeled Player Speed` = max_s,
                            `Max Modeled Player Acceleration` = max_s/ player_tau,
                            `Modeled Player Tau Value` = player_tau)
  player_profile

}
