#' Spectrogram data frame
#'
#' @description
#' This function uses the spectro() function from the *seewave* package
#' with automatic window length based on sampling rate and duration.
#'
#' @param wave A Wave object
#' @param floor Noise floor in dBFS.
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
spectro_df <- function(wave, floor = -35, overlap = 95, zeropad = 200){

  dyn <- as.numeric(floor)

  wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)

  if (wl %% 2 != 0) {
    wl <- wl + 1
  }

  spect <- wave |>
    seewave::spectro(
      wl = wl,
      ovlp = overlap,
      zp= zeropad,
      plot = FALSE,
      fftw = TRUE
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
      time = as.numeric(time),
      dyn = dyn
    )

  spect_df_floor <- spect_df |>
    mutate(
      amp_floor = case_when(
        amp < dyn ~ dyn,
        TRUE ~ amp
      )
    )

  return(spect_df_floor)
}
