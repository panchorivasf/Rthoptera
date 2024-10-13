#' Spectrogram with ggplot2 from a spectro data frame
#'
#' @param spectro_df A data frame obtained with the `spectro_df` function.
#' @param margin.l Left margin.
#' @param margin.r Right margin.
#' @param margin.t Top margin.
#' @param margin.b Bottom margin.
#'
#' @return A spectrogram plot.
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' spec_df <- spectro_df(coryphoda)
#' spectro_ggplot(spec_df)
#' }
spectro_ggplot <- function(spectro_df,
                           margin.l = 0,
                           margin.r = 0,
                           margin.t = 0,
                           margin.b = 0){

  spect_plot <- spectro_df |>
    ggplot(aes(time, freq)) +
    geom_raster(aes(fill = amp_floor)) +
    scale_fill_gradient(low = "white", high = "black",
                        limits = c(unique(spectro_df$dyn), max(spectro_df$amp)), na.value = "white") +
    scale_y_continuous(expand = c(0,0),
                       breaks = scales::breaks_pretty(),
                       labels = scales::label_number(accuracy = 1,
                                                     trim = TRUE,
                                                     zero.print = "")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       breaks = scales::breaks_pretty(),
                       labels = scales::label_number(accuracy = 0.1,
                                                     trim = TRUE,
                                                     zero.print = "")) +

    theme_minimal(base_size = 15) +
    theme_bw()+
    theme(
      plot.margin = margin(t=margin.t, r=margin.r, b=margin.b, l=margin.l, unit = 'pt'),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_line(colour = "black"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
      legend.position = "none"
    ) +
    labs(
      x = "Time (s)",
      y = "Frequency (kHz)",
      title = ""
    )

  return(spect_plot)
}

