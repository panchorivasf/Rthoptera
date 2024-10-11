#' Interactive Mean Spectrum
#'
#' @param wave A Wave object.
#' @param background Background color.
#' @param foreground Color for plot lines and spectral signatures.
#' @param hover_bgcolor Color for the hover background.
#' @param hover_fontcolor Color for the hover text.
#'
#' @return An interactive Mean Spectrum
#' @export
#' @importFrom seewave meanspec
#' @importFrom plotly plot_ly add_ribbons layout style config
#' @examples
#' \dontrun{
#' data(coryphoda)
#' meanspectrum_plotly(coryphoda)
#' }
meanspectrum_plotly <- function(wave,
                                background = '#274C77',
                                foreground = "white",
                                hover_bgcolor = "white",
                                hover_fontcolor = "black") {
  mean_spectrum <- seewave::meanspec(wave,
                                     f = wave@samp.rate,
                                     wl = 2048,
                                     ovlp = 50,
                                     plot = FALSE)
  mean_spectrum_df <- data.frame(
    freq = mean_spectrum[, 1],
    mean_amp = mean_spectrum[, 2]
  )

  plot_ly(data = mean_spectrum_df, x = ~freq, y = ~mean_amp, type = 'scatter', mode = 'lines', line = list(color = 'white')) %>%
    add_ribbons(ymin = 0, ymax = ~mean_amp, fillcolor = foreground, line = list(color = foreground)) %>%
    layout(
      title = "",
      xaxis = list(
        title = "Frequency (kHz)",
        titlefont = list(size = 10, color = foreground),
        tickfont = list(size = 10, color = foreground),
        ticks = "outside",
        tickcolor = foreground,
        tickwidth = 1,
        linecolor = foreground,
        ticklen = 5,
        automargin = TRUE,
        zeroline = FALSE,
        showline = TRUE
      ),
      yaxis = list(
        title = "Mean Amplitude",
        titlefont = list(size = 10, color = foreground),
        tickfont = list(size = 10, color = foreground),
        ticks = "outside",
        tickvals = pretty(mean_spectrum_df$mean_amp, n = 3),
        tickcolor = foreground,
        tickwidth = 1,
        ticklen = 5,
        rangemode= 'tozero',
        linecolor = foreground,
        zeroline = FALSE,
        showline = TRUE
      ),
      paper_bgcolor = background,
      plot_bgcolor = background,
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = max(mean_spectrum_df$freq),
          xref = "x",
          y0 = 0.1,
          y1 = 0.1,
          yref = "y",
          line = list(
            color = foreground,
            dash = "dot"
          )
        )
      ),
      margin = list(
        l = 50,
        r = 10,
        b = 60,
        t = 50
      ),
      showlegend = FALSE
    ) %>%
    config(displayModeBar = TRUE) %>%
    style(
      hovertemplate = paste0(
        "Frequency: %{x:.1f} kHz<br>",
        "<extra></extra>"
      )
    )
}
