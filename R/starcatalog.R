#' @title Catalog of 8774 stars and some information on them
#'
#' @description
#' Here is a paragraph describing the data.
#'
#' @docType data
#' @name starcatalog
#' @keywords datasets
#'
#' @source Astronomical data provided by Wesleyan's Astronomy department.
#' @format A data frame with 8774 rows and 8 variables:
#' \describe{
#'   \item{\code{name}}{character. name of the star.}
#'   \item{\code{alt_name}}{character. alternate name of the star, if applicable.}
#'   \item{\code{vmag}}{double. visual magnitude}
#'   \item{\code{multiple}}{character. indicates if the star is part of a binary (W) or multiple system (A or B).}
#'   \item{\code{class}}{character. class of the star.}
#'   \item{\code{bv_color}}{double. color of the star.}
#'   \item{\code{parallax}}{double. parallax of the star.}
#'   \item{\code{spect_type}}{character. spectral type of the star.}
#' }
"starcatalog"
