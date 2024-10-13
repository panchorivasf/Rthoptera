#' Oscillogram to Data Frame
#'
#' @param wave A Wave object.
#'
#' @return A data frame
#' @keywords internal
#' @noRd
#'
#' @examples
#' #' \dontrun{
#' oscillo_df(coryphoda)
#' }
oscillo_df <- function(wave){
  oscillo_data <- seewave::oscillo(wave, plot = FALSE)
  time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
  amplitude <- oscillo_data / max(abs(oscillo_data))
  oscillo_df <- data.frame(time = time, amplitude = amplitude)
  return(oscillo_df)
}
