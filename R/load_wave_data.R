#' Load all data items in Rthoptera
#'
#' @return All the Wave objects included in the package will be available in the environment.
#' @export
#'
#' @examples
#' \dontrun{
#' load_wave_data()
#' }
load_wave_data <- function() {
  # List all data items in the package
  data_list <- data(package = "Rthoptera")$results[, "Item"]

  # Load each data item
  for (data_name in data_list) {
    data(list = data_name, package = "Rthoptera")
  }

  message("All Waves in Rthoptera are now available in the R environment.")
}

# Example usage:
load_wave_data()
