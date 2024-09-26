#' Wave Data Frame
#'
#' @param wave A Wave object.
#' @param norm Logical. If true, the amplitude values are rescaled to a range of [-1,1].
#' @return A tibble data frame.
#' @export
#'
#' @examples wave_df(wave, norm = TRUE)
#' @importFrom magrittr %>%
#' @importFrom tuneR normalize
#' @importFrom seewave rmoffset
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select row_number
wave_df <- function(wave, norm = TRUE){

  if(norm){
    # Normalize to [-1,1] and remove DC offset (center to 0)
    wave <- tuneR::normalize(object = wave, unit = "1", center = TRUE)
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

