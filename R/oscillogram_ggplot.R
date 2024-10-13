#' Oscillogram with ggplot2
#'
#' @param wave A Wave object
#' @param y.title Character. A title for the Y-axis.
#' @param x.title Character. A title for the X-axis.
#'
#' @return An oscillogram plot.
#' @export
#'
#' @examples
#' \dontrun{
#' oscillgoram_ggplot(coryphoda)
#' }
oscillogram_ggplot <- function(wave,
                               y.title = "Relative Amplitude",
                               x.title = ""){

  oscillo_df <- oscillo_df(wave)

  oscillo_plot <- ggplot(oscillo_df, aes(x = time, y = amplitude)) +
    geom_line(color = "black") +
    theme_minimal(base_size = 15) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(n.breaks = 3, expand = c(0.03, 0.03)) +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 10, unit = "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.line.y = element_line(colour = "black"),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) +
    labs(y = y.title, x = x.title)

  return(oscillo_plot)
}

