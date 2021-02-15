#' Speed Acceleration Model Fit
#'
#'This function returns a data.frame containing a player's in-situ speed-acceleration values
#'
#' @param game_data
#'
#'@description
#'This function is based on the **in-situ** speed-acceleration model, originally suggested by Morin and
#'colleagues. It is currently written as efficiently as possible. Please keep in mind that larger
#'data set will require more time to process.
#'
#'The function returns a data.frame containing a player's in-situ max speed and acceleration values,
#'model fit (r_square), and number of observations (n_observation) kept in the final model output. This
#'funciton is written to return models with r_square values above 0.95. Well-fit models should return
#'approximately 45-65 observations.
#'
#'Processing time: approximately 0.01s per 1000 observations
#'
#' @return Modeled athlete max speed and max accel values, model fit (r_squared), and number of observations kept in the final model output.
#' @export
#'
#' @examples
#' data(player_a)
#' player_a_data ,- tracking_data(player_a$speed, player_a$accel)
#' player_a_insitu_model <- speed_accel_model(player_a_data)
#' print(player_a_insitu_model)
speed_accel <- function (game_data)
{
  UseMethod("speed_accel")
}

#' @export

speed_accel.default <- function(game_data) {


  df_temp <- speed_accel_values(game_data)

  fit <- lm(df.accel ~ df.speed, data = df_temp)
  r_square <- summary(fit)[[8]]
  y_int <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  x_int <- y_int/ abs(slope)

  while(r_square < 0.95) {

    df_temp$fit_predict <- y_int + (slope * df_temp$df.speed)
    df_temp$diff_predict <- abs(df_temp$df.accel - df_temp$fit_predict)
    df_temp <- df_temp[order(df_temp$diff_predict), ]
    df_temp <- head(df_temp, nrow(df_temp) -1)

    fit <- lm(df.accel ~ df.speed, data = df_temp)

    r_square <- summary(fit)[[8]]
    y_int <- coef(fit)[[1]]
    slope <- coef(fit)[[2]]
    x_int <- y_int/ abs(slope)

  }

  player_values <- data.frame(theoretical_max_accel = round(y_int, 1),
                              theoretical_max_speed = round(x_int, 1),
                              r_square = round(r_square, 2),
                              n_obervation = nrow(df_temp))

  player_values

}
