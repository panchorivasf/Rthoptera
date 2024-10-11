#' Interactive Spectrogram with Standardized Resolution
#'
#' @param wave A Wave object.
#' @param floor Background noise level, in dB FS.
#' @param background Background color.
#' @param foreground Color for plot lines and spectral signatures.
#' @param hover_bgcolor Color for the hover background.
#' @param hover_fontcolor Color for the hover text.
#' @param overlap FFT overlap.
#' @param zero_padding Number of zeroes to be added around the signal to improve frequency resolution.
#'
#' @return An interactive spectrogram.
#' @export
#' @importFrom seewave duration spectro
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom plotly plot_ly layout style
#' @examples
#' \dontrun{
#' data(coryphoda)
#' spectrogram_plotly(coryphoda)
#' }
spectrogram_plotly <- function(wave,
                               floor = -50,
                               background = '#274C77',
                               foreground = "white",
                               hover_bgcolor = "white",
                               hover_fontcolor = "black",
                               overlap = 50,
                               zero_padding = 0) {



  wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)
  # coerce wl to an even number
  if (wl %% 2 != 0) {
    wl <- wl + 1
  }

  spect <- wave |>
    seewave::spectro(
      wl = wl,
      ovlp = overlap,
      zp = zero_padding,
      plot = FALSE
    )

  colnames(spect$amp) <- spect$time
  rownames(spect$amp) <- spect$freq

  spect_df <- spect$amp |>
    as_tibble(rownames = "freq") |>
    pivot_longer(
      -freq,
      names_to = "time",
      values_to = "amp"
    ) |>
    mutate(
      freq = as.numeric(freq),
      time = as.numeric(time)
    )

  spect_df_floor <- spect_df |>
    mutate(
      amp_floor = ifelse(amp < floor, floor, amp)
    )

  spect_plot <- plot_ly(
    data = spect_df_floor,
    x = ~time,
    y = ~freq,
    z = ~amp_floor,
    type = "heatmap",
    colorscale = list(c(0, background), c(1, foreground)),
    zmin = floor,
    zmax = max(spect_df$amp),
    hoverinfo = "x+y",
    hovertemplate = paste(
      "Time: %{x:.3f} s<br>",
      "Freq: %{y:.1f} kHz<extra></extra>"
    ),
    showscale = FALSE
  ) %>%
    layout(
      xaxis = list(
        title = "Time (s)",
        titlefont = list(size = 10, color = foreground),
        tickfont = list(size = 10, color = foreground),
        tickcolor = foreground,
        linecolor = foreground,
        mirror = TRUE
      ),
      yaxis = list(
        title = "Frequency (kHz)",
        titlefont = list(size = 10, color = foreground),
        tickfont = list(size = 10, color = foreground),
        tickcolor = foreground,
        linecolor = foreground,
        mirror = TRUE
      ),
      paper_bgcolor = background,
      plot_bgcolor = background,
      margin = list(t = 25, r = 15, b = 55, l = 25),
      title = "",
      showlegend = FALSE
    ) %>%
    style(
      hoverlabel = list(
        bgcolor = hover_bgcolor,
        font = list(color = hover_fontcolor)
      )
    )

  return(spect_plot)
}
