#' Example Sound File
#'
#' The calling song of *Eumodicogryllus bordigalensis*, a Gryllidae from Europe, North Africa, and parts of Asia.
#' Available in Xeno-canto as: XC872608.
#' This sound was recorded at a sampling rate of 44.1 kHz, 16 bit depht.
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
#' data(eumodicogryllus)
#' summary(eumodicogryllus)
#' spectrum_plotly(eumodicogryllus)
#'
"eumodicogryllus"
