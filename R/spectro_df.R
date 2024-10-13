#' Spectrogram data frame
#'
#' @description
#' This function uses the spectro() function from the *seewave* package
#' with automatic window length based on sampling rate and duration.
#'
#' @param wave A Wave object
#' @param cutoff Amplitude cutoff in dB below the peak. Values below the cutoff will not be visible in the spectrogram.
#' @param overlap Window overlap (%).
#' @param zeropad Zero padding.
#'
#' @return A data frame with the results of a STFT applied to the wave,
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' spectro_df(coryphoda)
#' }
spectro_df <- function(wave, cutoff = -35, overlap = 95, zeropad = 200){


  wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)

  if (wl %% 2 != 0) {
    wl <- wl + 1
  }

  spect <- wave %>%
    seewave::spectro(
      wl = wl,
      ovlp = overlap,
      zp= zeropad,
      plot = FALSE,
      fftw = TRUE
    )

  colnames(spect$amp) <- spect$time
  rownames(spect$amp) <- spect$freq

  spect_df <- spect$amp |> as_tibble(rownames = "freq") |>
    pivot_longer(-freq, names_to = "time", values_to = "amp") |>
    mutate(freq = as.numeric(freq), time = as.numeric(time))

  dyn <- as.numeric(cutoff)
  spect_df_cutoff <- spect_df |> mutate(amp_cutoff = ifelse(amp < dyn, dyn, amp))


  return(spect_df_cutoff)
}
