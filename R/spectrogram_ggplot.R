#' Standardized Spectrogram plus Mean Power Spectrum
#'
#' @param wave A `Wave` object from the `tuneR` package, representing the audio signal to be analyzed.
#' @param meanspec Logical. If `TRUE`, a mean power spectrum is plotted alongside the spectrogram. The spectrum is flipped and positioned to the right.
#' @param widths A numeric vector of length 2 specifying the relative widths of the spectrogram and the mean power spectrum when `meanspec = TRUE`. Defaults to `c(10, 1)`.
#' @param cutoff Numeric. The amplitude cutoff in dB relative to the peak amplitude. Any amplitude below this cutoff will not be visible in the spectrogram. Defaults to `-35`.
#' @param scale.type Character. The scale used for the amplitude. Either `"dB"` for decibel scaling or `"Linear"` for a linear amplitude scale. Defaults to `"dB"`.
#' @param overlap Numeric. The percentage of overlap between successive windows for spectrogram calculation. Defaults to `95`.
#' @param zeropad Numeric. The number of zero-padding points to add to each window to increase frequency resolution. Defaults to `200`.
#' @param margin.l Numeric. The left margin (in points) around the plot. Defaults to `10`.
#' @param margin.r Numeric. The right margin (in points) around the plot. Defaults to `0`.
#' @param margin.t Numeric. The top margin (in points) around the plot. Defaults to `0`.
#' @param margin.b Numeric. The bottom margin (in points) around the plot. Defaults to `10`.
#' @param show.axis.titles Logical. If `TRUE`, axis titles for time and frequency will be displayed on the spectrogram. Defaults to `TRUE`.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom patchwork plot_layout
#'
#' @return A `ggplot2` object representing the spectrogram, with the option to include a mean power spectrum on the right side.
#' @export
#'
#' @examples
#' \dontrun{
#' data(coryphoda)
#' # Create a spectrogram with a mean power spectrum for a wave object:
#' spectrogram_ggplot(coryphoda, meanspec = TRUE, cutoff = -40, scale.type = "dB")
#'
#' # Create a spectrogram without the mean power spectrum:
#' spectrogram_ggplot(coryphoda, meanspec = FALSE)
#' }
spectrogram_ggplot <- function(wave,
                               meanspec = TRUE,
                               widths = c(10,1),
                               cutoff = -35,
                               scale.type = "dB",
                               overlap = 95,
                               zeropad = 200,
                               margin.l = 10,
                               margin.r = 0,
                               margin.t = 0,
                               margin.b = 10,
                               show.axis.titles = TRUE) {

  spect_df <- spectro_df(wave,
                         cutoff = cutoff,
                         overlap = overlap,
                         zeropad = zeropad)

  dyn <- as.numeric(cutoff)

  spect_plot <- spect_df %>%
    ggplot(aes(time, freq)) +
    geom_raster(aes(fill = amp_cutoff)) +
    scale_fill_gradient(low = "white", high = "black",
                        limits = c(dyn, max(spect_df$amp)), na.value = "white") +
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
    )

  if (show.axis.titles){
    spect_plot <- spect_plot +
      labs(
        x = "Time (s)",
        y = "Frequency (kHz)",
        title = ""
      )

  }



  if(meanspec){

    spectrum_plot <- meanspec_ggplot(wave, scale.type = scale.type)

    combined_plot <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = widths)

    return(combined_plot)

  } else {

    return(spect_plot)

  }
}
