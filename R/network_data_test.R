#' network_data_test
#'
#' network_data_test includes 4 columns, therefore 2 sets of network data in one data frame. There are 13 rows of random data. Columns 1 and 2 are of the form "Neuron 1" -> "Neuron 2". Columns 3 and 4 are of the form "A" -> "B".
#'
#' @format A data frame with 13 rows and 4 variables:
#' \describe{
#'   \item{\code{from}}{character. Set 1 Initial point for network plot. Format: "Neuron 1", "Neuron 2", etc.}
#'   \item{\code{to}}{character. Set 1 Target point for network plot. Format: "Neuron 1", "Neuron 2", etc.}
#'   \item{\code{inital_test}}{character. Set 2 Initial point for network plot. Format: "A", "B", etc.}
#'   \item{\code{target_test}}{character. Set 2 Target point for network plot. Format: "A", "B", etc.}
#' }
"network_data_test"
