
# midsprint helper functions ----------------------------------------------

# These functions feed into player_plot_values() and are not exported
# No documentation is provided for these function
# All function information is included within the function as a comment

# Built for the original big data bowl 2021 submission:
# 5 Seconds or Less: Modeling player biomechanics to predict pass outcomes
# https://www.kaggle.com/haljordan/modeling-player-biomechanics

# Thank you to DChu for help on these functions.

distance_speed <- function(player_profile, current_speed) {

  # returns the distance travelled to reach a given speed
  # starting velocity does not have to be 0 m/s

  # Thank you to DChu for help on this function.

  df <- player_profile
  max_speed <- df[[1]]
  player_tau <- df[[3]]

  split_time <- time_speed(player_profile, current_speed)

  pos_vel <- max_speed * (split_time + player_tau * exp(-split_time/player_tau)) - (max_speed * player_tau)
  return(pos_vel)

}

time_distance <- function(player_profile, distance) {

  # returns the time elapsed to travel a given distance
  # starting velocity must be 0 m/s

  # Thank you to DChu for help on this function.

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  time_pos <- player_tau * lamW::lambertW0(-exp(-1 - distance/(player_tau * max_speed))) + distance/max_speed + player_tau
  return(time_pos)

}

time_to_dist_given_speed <- function(player_profile, distance, current_speed) {

  # returns the time elapsed to travel a given distance
  # starting velocity does not have to be 0 m/s

  # cleaner call is time_to_position
  # this function may become depreciated in future updates

  # Thank you to DChu for help on this function.

  df <- player_profile
  max_speed <- df[[1]]
  player_tau <- df[[3]]

  pos_0 <- distance_speed(df, current_speed)

  dist_dif <- time_distance(df, distance + pos_0) - time_distance(df, pos_0)
  return(dist_dif)

}

time_to_position <- function(player_profile, current_speed, distance) {

  # returns the time elaplsed to travel a given distance
  # starting velocity does not have to be 0 m/s

  # alternative call to time_to_dist_given_speed
  # cleaner

  # Thank you to DChu for help on this function.

  time_to_dist <- time_to_dist_given_speed(player_profile, distance, current_speed)
  round(time_to_dist, 2)


}

time_speed <- function(player_profile, current_speed) {

  # returns the time elapsed to reach a given speed
  # starting velocity does not have to be 0 m/s

  # Thank you to DChu for help on this function.

  max_speed <- player_profile[[1]]
  player_tau <- player_profile[[3]]

  time_vel <- -log(-(current_speed/max_speed -1)) * player_tau
  return(time_vel)

}
