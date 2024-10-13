#' Multiplot including Spectrogram, Mean Power Spectrum, and
#'
#' @param wave A `Wave` object.
#' @param floor The noise floor in dBFS.
#' @param overlap Window overlap (%).
#' @param zeropad Zero padding.
#' @param scale Scale for the amplitude. Either "dB" or "Linear".
#'
#' @return A plot with aligned spectrogram, mean spectrum, and oscillogram.
#' @export
#'
#' @examples
#' \dontrun{
#' multiplot_ggplot(coryphoda)
#' }
multiplot_ggplot <- function(wave,
                             floor = -35,
                             overlap = 95,
                             zeropad = 200,
                             scale = "dB",
                             heights = c(8,2)) {

  floor = as.numeric(floor)

  combined_spect_mean <- spectrogram_ggplot(wave,
                                            floor = floor,
                                            overlap = overlap,
                                            zeropad = zeropad,
                                            scale = scale)


  oscillo_plot <- oscillogram_ggplot(wave)

  final_plot <- (combined_spect_mean / (oscillo_plot + plot_spacer() + plot_layout(ncol = 2, widths = c(5, 0.5)))+plot_layout(heights = heights))

  return(final_plot)
}
