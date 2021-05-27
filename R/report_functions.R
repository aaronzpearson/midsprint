
#' Start Report
#'
#' @param report_name Name of the report
#'
#' @return A player report for 1+ athletes
#'
#' @description
#'
#' The report functions return athlete information on 1+ pages. Each line creates a new page in the .pdf that is saved in your working directory.
#'
#' Formatting the functions in this manner allows you to create a player report for an individual, a position group, or the entire team with minimal effort.
#'
#' Please note: you must work sequentially as outlined in the supporting .pdf document. If you do not and you run into errors, try running save_report and start again. It is imperative that save_report be called otherwise the report will not be saved and can interfere with other reports you plan to produce.
#'
#' @export
start_report <- function(report_name) {

  save_as <- paste0(report_name, ".pdf")
  pdf(width = 12, height = 8, save_as)

}

#' @export
#' @rdname start_report
save_report <- function() {

  dev.off()

}

#' @export
#' @rdname start_report
speed_accel_report <- function(game_data, ...) {

  p <- speed_accel_plot(game_data, ...)
  q <- speed_accel_time_distance_plot(game_data, ...)

  pq <- p + theme(legend.position = "bottom") +
    ggtitle("Player Speed-Acceleration Report") +
    inset_element(q + theme_bw() +
                    theme(legend.position = "none",
                          plot.title = element_blank(),
                          plot.subtitle = element_blank(),
                          legend.title = element_text(""),
                          panel.background = element_rect(fill = alpha("white", 0.5))),
                  left = 0.5,
                  bottom = 0.5,
                  right = unit(1, 'npc') - unit(1, 'cm'),
                  top = unit(1, 'npc') - unit(1, 'cm'))

  suppressMessages(suppressWarnings(print(pq)))

}

#' @export
#' @rdname start_report
accel_speed_report <- function(game_data, ...) {

  p <- speed_accel_report(game_data, ...)
  p

}

#' @export
#' @rdname start_report
game_report <- function(game_profile, ...) {

  p <- game_player_plot(game_profile, ...)
  q <- game_time_distance_plot(game_profile, ...)

  pq <- p + theme(legend.position = "bottom") +
    ggtitle("In-Game Speed and Acceleration Report") +
    inset_element(q + theme_bw() +
                    theme(legend.position = "none",
                          plot.title = element_blank(),
                          plot.subtitle = element_blank(),
                          legend.title = element_text(""),
                          plot.background = element_rect(fill = scales::alpha("white", 0.2))),
                  left = 0.5,
                  bottom = 0.5,
                  right = unit(1, 'npc') - unit(1, 'cm'),
                  top = unit(1, 'npc') - unit(1, 'cm'))

  suppressMessages(suppressWarnings(print(pq)))

}

#' @export
#' @rdname start_report
combine_report <- function(combine_profile, ...) {

  p <- combine_player_plot(combine_profile, ...)
  q <- combine_time_distance_plot(combine_profile, ...)

  pq <- p + theme(legend.position = "bottom") +
    ggtitle("Combine Speed and Acceleration Report") +
    inset_element(q + theme_bw() +
                    theme(legend.position = "none",
                          plot.title = element_blank(),
                          plot.subtitle = element_blank(),
                          legend.title = element_text(""),
                          plot.background = element_rect(fill = scales::alpha("white", 0.2))),
                  left = 0.5,
                  bottom = 0.5,
                  right = unit(1, 'npc') - unit(1, 'cm'),
                  top = unit(1, 'npc') - unit(1, 'cm'))
  suppressMessages(suppressWarnings(print(p)))

}
