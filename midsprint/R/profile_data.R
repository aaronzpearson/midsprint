#' Profile Data
#'
#' @param data
#' @param speed_column
#' @param acceleration_column
#'
#' @return Profiling data from positional tracking data
#' @export
#'
#' @examples
#' \dontrun {tracking_data <- data.frame(speed = rnorm(100, 4, 1),
#' acceleration = rnorm(100, 2, 0.2))
#' dat <- profile_data(tracking_data, "speed", "acceleration")
#' head(dat)}
#'
profile_data <- function(data, speed_column, acceleration_column) {

  speed_col = data[, grep(as.character(speed_column), colnames(data))]
  accel_col = data[, grep(as.character(acceleration_column), colnames(data))]

  return(data.frame(speed = speed_col,
                    accel = accel_col))

}
