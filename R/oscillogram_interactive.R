#' Interactive Oscillogram
#'
#' This function generates an interactive oscillogram plot for a given wave object using Plotly.
#' The plot displays the waveform over time with customizable options for the title and line color.
#'
#' @param wave A wave object containing the audio data to be plotted.
#' @param title A string specifying the title of the plot. Default is an empty string.
#' @param line_color A string specifying the color of the waveform line. Default is 'black'.
#'
#' @return A Plotly object representing the interactive oscillogram.
#' @export
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly
#' @examples
#' \dontrun{
#' oscillogram_interactive(coryphoda_wave, title = "Coryphoda albidicollis")
#' }
oscillogram_interactive <- function(wave, title = "", line_color = 'black') {


  df <- wave_df(wave)

  # Create an interactive plot with plotly
  p <- plot_ly(data = df, x = ~time, y = ~amplitude,
               type = 'scatter', mode = 'lines',
               line = list(shape = "spline",
                           color = line_color)) %>%
    layout(title = title,
           xaxis = list(title = 'Time'),
           yaxis = list(title = 'Amplitude'))

  return(p)
}

