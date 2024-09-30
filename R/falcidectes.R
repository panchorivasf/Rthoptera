#' Example Sound File 2
#'
#' The calling song of *Falcidectes sp.*, a Tettigoniidae from Central Chile.
#'
#' This mono file was recorded at a sampling rate of 96 kHz, 16 bit depth.The recording lasts 4.3 seconds.
#' The sound file has been imported into an R object using
#' the \code{\link[tuneR]{readWave}} function from the \pkg{tuneR} package.
#'
#' @format An object of class \code{\link[tuneR]{Wave}}. The Wave object contains:
#' \describe{
#'   \item{left}{The left audio channel (if stereo).}
#'   \item{samp.rate}{The sampling rate in Hz.}
#'   \item{bit}{The bit depth (e.g., 16 for CD quality).}
#'   \item{duration}{The total duration of the sound file in seconds.}
#' }
#'
#' @source Original recording by Francisco Rivas Fuenzalida.
#'
#' @examples
#' \dontrun{
#' data(coryphoda22C)
#' summary(coryphoda22C)
#' spectrum_plotly(coryphoda22C)
#'}
"falcidectes"
