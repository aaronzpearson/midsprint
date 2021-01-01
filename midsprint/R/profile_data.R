#' Profile Data
#'
#' @param speed_column
#' @param acceleration_column
#'
#' @return Profiling data from positional tracking data
#' @export
#'
#' @examples
#' \dontrun {tracking_data <- data.frame(speed = rnorm(100, 4, 1),
#' acceleration = rnorm(100, 2, 0.2))
#' dat <- profile_data(tracking_data$speed, tracking_data$acceleration)
#' head(dat)}
#'
profile_data <- function(speed, acceleration) {
  
  df <- data.frame(speed = speed,
                   accel = acceleration)
  
  colnames(df) <- c("speed", "accel")
  
  
  return(df)
  
}
