#' Example Sound File
#'
#' A single calling song of *Coryphoda albidicollis*, a Phaneropterinae from Central Chile.
#'
#' This sound was recorded at a sampling rate of 96 kHz. The ambient temperature was 22.6 degrees Celsius.
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
#' data(coryphoda)
#' summary(coryphoda)
#' spectrum_plotly(coryphoda)
#' }

"coryphoda"
