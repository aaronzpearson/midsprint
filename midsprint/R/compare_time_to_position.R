#' Comparing Two Players to Reach a Position
#'
#' @param time_to_position_1
#' @param time_to_position_2
#'
#' @return
#' @export
#'
#' @examples
compare_time_to_position <- function(time_to_position_1, time_to_position_2) {

  str1 <- "Player 1:"
  str2 <- "Player 2:"
  seconds <- "seconds."

  result1 <- paste(str1, time_to_position_1, seconds)
  result2 <- paste(str2, time_to_position_2, seconds)

  c(result1, result2)

}
