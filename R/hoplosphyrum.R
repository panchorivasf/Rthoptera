#' Example Sound File 3
#'
#' The calling song of *Hoplosphyrum griseum*, a Mogoplistidae from Central Chile.
#'
#' This recording has sampling rate of 48 kHz.
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
#' data(hoplosphyrum)
#' summary(hoplosphyrum)
#' spectrum_plotly(hoplosphyrum)
#'
"hoplosphyrum"
