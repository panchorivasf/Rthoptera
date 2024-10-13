#' Multiplot including Spectrogram, Mean Power Spectrum, and
#'
#' @param wave A `Wave` object. The sound data to be plotted.
#' @param cutoff Numeric. Amplitude cutoff in dB below the peak. Values below the cutoff will not be visible in the spectrogram. Default is -35 dB.
#' @param overlap Numeric. Window overlap percentage for the spectrogram. Default is 95.
#' @param zeropad Numeric. Zero padding for FFT in the spectrogram. Default is 200.
#' @param scale.type Character. Scale for the amplitude in the spectrogram, either "dB" or "Linear". Default is "dB".
#' @param heights Numeric vector. The relative heights of the spectrogram/mean spectrum plot and the oscillogram. Default is `c(7,3)`.
#' @param oscillo.label Character. A title for the Y-axis of the oscillogram. Default is an empty string.
#'
#' @return A plot with aligned spectrogram, mean spectrum, and oscillogram.
#'
#' @import patchwork
#' @export
#'
#' @examples
#' \dontrun{
#' data(coryphoda)
#' multiplot_ggplot(coryphoda)
#' }
multiplot_ggplot <- function(wave,
                             cutoff = -35,
                             overlap = 95,
                             zeropad = 200,
                             scale.type = "dB",
                             heights = c(7,3),
                             oscillo.label = "") {

  cutoff = as.numeric(cutoff)

  combined_spect_mean <- spectrogram_ggplot(wave,
                                            cutoff = cutoff,
                                            overlap = overlap,
                                            zeropad = zeropad,
                                            scale.type = scale.type)

  oscillo_plot <- oscillogram_ggplot(wave, y.title = oscillo.label)

  final_plot <- combined_spect_mean /
    (oscillo_plot + plot_spacer() + plot_layout(ncol = 2, widths = c(5, 0.45))) +
    plot_layout(heights = heights)

  return(final_plot)
}
