#' Spectrum Plot
#'
#' This function generates a spectrum plot with various customizable options, including window size, color schemes, and the ability to display statistical summaries and frequency markers. The plot can display both dB and linear amplitude scales, and bandwidths based on thresholds can be shown.
#'
#' @param wave A wave object containing the audio data.
#' @param win.size Numeric, the window size for the FFT. If `NULL`, it will be set automatically based on the sample rate.
#' @param ovlp Numeric, the overlap between successive windows in percentage. Default is 50.
#' @param x.breaks Numeric, the number of breaks on the x-axis (frequency axis). Default is 6.
#' @param y.position Character, the position of the y-axis, either "left" or "right". Default is "left".
#' @param x.position Character, the position of the x-axis, either "top" or "bottom". Default is "bottom".
#' @param flip Logical, if `TRUE`, the plot will be flipped. Default is `FALSE`.
#' @param color.db Character, color for the dB amplitude plot. Default is "grey30".
#' @param color.linear Character, color for the linear amplitude plot. Default is "black".
#' @param color.carrier Character, color for the carrier frequency marker. Default is "white".
#' @param color.threshold Character, color for the threshold line. Default is "white".
#' @param color.bandwidth Character, color for the bandwidth lines. Default is "white".
#' @param fun Character, the summary function to apply to the spectrum, one of "mean", "median", "var", "sd". Default is "mean".
#' @param wn Character, the window type for the FFT, one of "bartlett", "blackman", "flattop", "hamming", "hanning", "rectangle". Default is "hanning".
#' @param show.x.title Logical, whether to show the x-axis title. Default is `TRUE`.
#' @param show.y.title Logical, whether to show the y-axis title. Default is `TRUE`.
#' @param add.params Logical, whether to add window parameters as a label on the plot. Default is `FALSE`.
#' @param add.summary Logical, whether to add summary statistics as a label on the plot. Default is `TRUE`.
#' @param plot.title Character, title of the plot. Default is an empty string.
#' @param italic.title Logical, whether to italicize the plot title. Default is `FALSE`.
#' @param fmin Numeric, minimum frequency (in kHz) to plot. If `NULL`, the entire frequency range is plotted.
#' @param fmax Numeric, maximum frequency (in kHz) to plot. If `NULL`, it defaults to the Nyquist frequency.
#' @param linewidth Numeric, the width of the lines in the plot. Default is 1.
#' @param total.bandwidth Logical, whether to calculate the total bandwidth, ignoring gaps. Default is `FALSE`.
#' @param show.lines Logical, whether to show lines for the carrier frequency and bandwidths. Default is `FALSE`.
#'
#' @return A list containing a ggplot2 object for the spectrum plot.
#' @import ggplot2
#' @importFrom seewave meanspec spec sh sfm
#' @importFrom scales breaks_pretty label_number
#' @noRd
#'
#' @examples
#'  \dontrun{
#' spectrum_plot(wave, win.size = 512, x.breaks = 5, fmin = 1, fmax = 10)
#' }
spectrum_plot <- function(wave,
                           win.size = NULL,
                           ovlp = 50,
                           x.breaks = 6,
                           y.position = "left",
                           x.position = "bottom",
                           flip = FALSE,
                           color.db = "grey30",
                           color.linear = 'black',
                           color.carrier = "white",
                           color.threshold = "white",
                           color.bandwidth = "white",
                           fun = "mean",
                           wn = "blackman",
                           show.x.title = TRUE,
                           show.y.title = TRUE,
                           add.params = FALSE,
                           add.summary = TRUE,
                           plot.title = "",
                           italic.title = FALSE,
                           fmin = NULL,
                           fmax = NULL,
                           linewidth = 1,
                           total.bandwidth = FALSE,
                           show.lines = FALSE) {


  if(is.null(win.size)){

    # Standardized window length according to sample rate
    wl = round(wave@samp.rate * 1e-2)

    if (wl %% 2 != 0) {
      wl <- wl + 1
    }

  } else {

    wl = win.size

    if (wl %% 2 != 0) {
      wl <- wl + 1
    }

  }


  # Automatically set fmax to Nyquist frequency if not provided
  if (is.null(fmax) || fmax == 0) {
    fmax <- wave@samp.rate / 2 / 1000  # Convert to kHz
  }

  # Ensure fmin and fmax are numeric
  fmin <- as.numeric(fmin)
  fmax <- as.numeric(fmax)

  # Calculate the dB scale spectrum
  meanspec_data_dB <- seewave::meanspec(wave,
                                        f = wave@samp.rate,
                                        wl = wl,
                                        ovlp = ovlp,
                                        plot = FALSE,
                                        norm = TRUE,  # Normalize to max amplitude for dB scale
                                        dB = "max0",
                                        wn = wn,
                                        FUN = fun,
                                        flim = c(fmin, fmax))

  # Calculate the linear scale spectrum
  meanspec_data_linear <- seewave::meanspec(wave,
                                            f = wave@samp.rate,
                                            wl = wl,
                                            ovlp = ovlp,
                                            plot = FALSE,
                                            norm = TRUE,  # Normalize to max amplitude for linear scale
                                            wn = wn,
                                            FUN = fun,
                                            flim = c(fmin, fmax))

  # Convert data to data frames
  meanspec_data <- data.frame(
    freq = meanspec_data_dB[, 1],
    mean_amp_dB = meanspec_data_dB[, 2],
    mean_amp_linear = meanspec_data_linear[, 2]
  )

  # Min-max normalization for both dB and linear scales
  meanspec_data$norm_amp_dB <- (meanspec_data$mean_amp_dB - min(meanspec_data$mean_amp_dB)) /
    (max(meanspec_data$mean_amp_dB) - min(meanspec_data$mean_amp_dB))

  meanspec_data$norm_amp_linear <- (meanspec_data$mean_amp_linear - min(meanspec_data$mean_amp_linear)) /
    (max(meanspec_data$mean_amp_linear) - min(meanspec_data$mean_amp_linear))

  # Carrier frequency (maximum amplitude frequency)
  carrier_freq <- meanspec_data$freq[which.max(meanspec_data$mean_amp_dB)]

  # Calculate low and high frequencies based on the 0.1 threshold
  if (total.bandwidth) {
    # Find the last frequencies below the threshold, ignoring gaps
    low_freq <- min(meanspec_data$freq[meanspec_data$freq < carrier_freq & meanspec_data$norm_amp_linear >= 0.1])
    high_freq <- max(meanspec_data$freq[meanspec_data$freq > carrier_freq & meanspec_data$norm_amp_linear >= 0.1])
  } else {
    # Regular bandwidth: Find the first crossing below the threshold, then select the next valid point
    # For low frequency, search leftward from the carrier
    low_freq_index <- max(which(meanspec_data$freq < carrier_freq & meanspec_data$norm_amp_linear <= 0.1))
    if (!is.na(low_freq_index) && low_freq_index < length(meanspec_data$freq)) {
      low_freq <- meanspec_data$freq[low_freq_index + 1]
    } else {
      low_freq <- fmin  # Default to fmin if no suitable frequency is found
    }

    # For high frequency, search rightward from the carrier
    high_freq_index <- min(which(meanspec_data$freq > carrier_freq & meanspec_data$norm_amp_linear <= 0.1))
    if (!is.na(high_freq_index) && high_freq_index > 1) {
      high_freq <- meanspec_data$freq[high_freq_index - 1]
    } else {
      high_freq <- fmax  # Default to fmax if no suitable frequency is found
    }
  }


  # Calculate the spectral indices within the frequency range of interest
  spec_linear <- seewave::spec(wave, PSD=TRUE, plot = FALSE, flim = c(low_freq, high_freq))
  # Spectral entropy:
  spec.ent <- seewave::sh(spec_linear)
  # Spectral flatness
  spec.flat <- seewave::sfm(spec_linear)

  # Create the plot with normalized scales
  spectrum_plot <- ggplot(meanspec_data, aes(x = freq)) +
    theme_minimal(base_size = 15)

  # Primary y-axis with normalized dB scale
  spectrum_plot <- spectrum_plot +
    geom_ribbon(aes(ymin = 0, ymax = norm_amp_dB), fill = color.db, alpha = 0.9) +
    geom_ribbon(aes(ymin = 0, ymax = norm_amp_linear), fill = color.linear, alpha = 0.9) +
    scale_y_continuous(
      name = "Normalized Amplitude",
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      expand = expansion(mult = c(0, .1)),
      position = y.position
    )

  # Set x-axis parameters
  spectrum_plot <- spectrum_plot +
    scale_x_continuous(limits = c(fmin, fmax),  # Explicitly set x-axis limits
                       expand = c(0, 0),
                       position = x.position,
                       breaks = scales::breaks_pretty(n = x.breaks),
                       labels = scales::label_number(zero.print = "")) +
    theme_bw() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.ticks.y = element_line(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      legend.position = "none",
      plot.title = element_text(face = if (italic.title) "italic" else "plain")
    ) +
    labs(
      x = if (show.x.title) "Frequency (kHz)" else NULL,
      title = plot.title
    )

  if(show.lines){

    spectrum_plot <- spectrum_plot +
      geom_vline(xintercept = carrier_freq, color = color.carrier, linetype = "solid", size = linewidth) +
      annotate("segment", x = low_freq, xend = high_freq, y = 0.1, yend = 0.1, color = color.threshold,
               linetype = "dashed", size = linewidth) +
      geom_vline(xintercept = low_freq, color = color.bandwidth, linetype = "solid", size = linewidth) +
      geom_vline(xintercept = high_freq, color = color.bandwidth, linetype = "solid", size = linewidth)

  }

  if (add.params) {

    if(is.null(win.size)){

      winsize <- paste0("Window Size: ", wl, " (auto)")
    } else {
      winsize <- paste0("Widow Size: ", wl)
    }
    # Parameters annotation
    params_text1 <-
      paste0(winsize,
             "\nSummary Function: ", fun,
             "\nFilter Function: ", wn,
             "\nWindow Overlap:", ovlp, "%"
      )

    spectrum_plot <- spectrum_plot +
      annotate("label",
               x = Inf,
               y = Inf,
               hjust = 1.05,
               vjust = 1.05,
               label = params_text1,
               size = 4,
               color = "black",
               fill = "white",
               alpha = 0.8)

  }

  if(add.summary){

    # Summary Statistics annotations
    measurements <-
      paste0("Entropy: ", round(spec.ent, 2),
             "\nFlatness: ", round(spec.flat, 2),
             "\nCarrier: ", round(carrier_freq, 2), " kHz",
             "\nHigh F.: ", round(high_freq, 2), " kHz",
             "\nLow F.: ", round(low_freq, 2), " kHz"
      )

    spectrum_plot <- spectrum_plot +
      annotate("label",
               x= -Inf,
               y = Inf,
               hjust = -0.05,
               vjust = 1.05,
               label = measurements,
               size = 4,
               color = "black",
               fill = "white",
               alpha = 0.8)
  }

  if (flip) {
    spectrum_plot <- spectrum_plot +
      coord_flip()
  }

  return(list(spectrum_plot))
}
