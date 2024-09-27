#' Example Sound File
#'
#' The calling song of *Tettigonia cantans*, a Tettigoniidae from Europe and Central Asia.
#' Available in Xeno-canto as: XC922654.
#' Sampling rate: 44.1 kHz.
#' Bit depth: 16 bit.
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
#' @source Original recording by Cesare Brizio.
#'
#' @examples
#' data(tettigonia)
#' summary(tettigonia)
#' spectrum_plotly(tettigonia)
#'
"tettigonia"
