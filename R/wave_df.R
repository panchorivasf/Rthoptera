#' Extract the waveform values as a table
#'
#' @param wave A Wave object.
#' @param norm Logical. If true, the amplitude values are rescaled to a range of [-1,1].
#' @return A tibble data frame.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom tuneR normalize
#' @importFrom seewave rmoffset
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select row_number
#' @examples
#'  \dontrun{
#' data(coryphoda)
#' wave_df(coryphoda)
#'}
wave_df <- function(wave, norm = TRUE){

  if(norm){
    # Normalize to [-1,1] and remove DC offset (center to 0)
    wave <- tuneR::normalize(wave, unit = "1", center = TRUE)
  } else {
    # Just remove DC offset
    wave <- rmoffset(wave, output = "Wave")
  }

  srate <- wave@samp.rate

  amplitude <- wave@left

  tbl <- tibble(amplitude = amplitude)
  tbl <- tbl %>%
    mutate(index = row_number(),
           time = (index-1 ) / srate)
  tbl <- tbl %>%
    select(c(amplitude, time))

  return(tbl)
}

