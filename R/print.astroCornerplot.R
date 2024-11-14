#'@title Astronomical Cornerplots
#'@description Create cornerplots for astronomy data for an unlimited number of paired variables.
#'@export
#'@param data a data frame
#'@param varlist character, list with all numeric variables to create the cornerplot and histograms with. defaults to all variables in the dataset
#'@param contour logical, specify whether to overlay contour plot on 2d histogram
#'@param fill fill color for histograms, defaults to mediumpurple
#'@param bins number of bins for all histograms, defaults to 70
#'@param palette color palette for 2d histogram scale, defaults to inferno
#'@param contourBins number of bins for contour plot, defaults to 5
#'@returns corner plot with 2d histogram showing intersection of both variables and regular histogram for each variable
#'@import dplyr
#'@import ggplot2
#'@import patchwork
#'@import colorspace
#'@import rlang
#'@examples
#'astroCornerplot(starcatalog)
#'astroCornerplot(starcatalog, contour=FALSE, fill="red4", palette = "Reds", bins=40)
