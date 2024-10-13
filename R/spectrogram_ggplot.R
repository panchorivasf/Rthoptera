#' Standardized Spectrogram plus Mean Power Spectrum
#'
#' @param wave A wave object.
#' @param meanspec Logical. If TRUE, a flipped mean power spectrum is added on the right side of the spectrogram.
#' @param floor Noise floor in dBFS.
#' @param scale Scale for the amplitude. Either "dB" or "Linear".
#' @param overlap Window overlap (%).
#' @param zeropad Zero padding.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom patchwork plot_layout
#'
#' @return A spectrogram plot with automatic window length and optionally a mean power spectrum.
#' @export
#'
#' @examples
#' \dontrun{
#' spectrogram_ggplot()}
spectrogram_ggplot <- function(wave,
                               meanspec = TRUE,
                               floor = -35,
                               scale = "dB",
                               overlap = 95,
                               zeropad = 200) {

  spect_df <- spectro_df(wave, floor = floor, overlap = overlap, zeropad = zeropad)

  if(meanspec){
    spect_plot <- spectro_ggplot(spect_df, margin.l = 10, margin.r = 0, margin.t = 0, margin.b = 10)

    spectrum_plot <- meanspec_ggplot(wave, scale = scale)

    combined_plot <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = c(10, 1))

    return(combined_plot)

  } else {
    spect_plot <- spectro_ggplot(spect_df, margin.l = 10, margin.r = 10, margin.t = 0, margin.b = 10)
    return(spect_plot)
  }
}
