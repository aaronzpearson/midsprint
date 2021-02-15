#' Speed-Accel Model Observations
#'
#' This model returns observations that were kept for the final in-situ speed-acceleration plot
#'
#' @param game_data
#'
#' @return Back-end observations fed into speec_accel_plot()
#'
#' @noRd
speed_accel_points <- function (game_data)
{
  UseMethod("speed_accel_points")
}


speed_accel_points.default <- function(game_data) {

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

  model_points <- df_temp
  model_points
}
