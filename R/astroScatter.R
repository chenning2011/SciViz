#'@title Astronomical Scatterplots
#'@description Create scatterplots for astronomy data, optimized for a large number of points
#'@export
#'@param data a data frame
#'@param x string, a numeric variable
#'@param y string, a numeric variable
#'@param size (optional) size for the points on the graph. defaults to 0.5
#'@param alpha (optional) transparency for the points on the graph. defaults to 0.5
#'@param color (optional) string, color of the points. takes either a variable from the dataset or the name of a color.
#'@param palette (optional) string, color palette for points, if color provided is a variable. defaults to agSunset. see `scale_color_discrete_sequential` function in the colorspace package for palette options.
#'@param group (optional) string, facet variable for the graphs.
#'@returns scatter plot with all specified variables
#'@import ggplot2
#'@import dplyr
#'@import colorspace
#'@examples
#'astroScatter(starcatalog, "vmag", "bv_color")
#'astroScatter(starcatalog, "vmag", "bv_color", color = "multiple", palette = "Terrain", size = 0.2, alpha = 0.7)
#'astroScatter(starcatalog, "vmag", "bv_color", color = "mediumpurple", palette = "Terrain", size = 0.2, alpha = 0.7)
#'astroScatter(starcatalog, "vmag", "bv_color", group = "multiple")

astroScatter <- function(data, x, y, size=0.5, alpha=0.5, color=NULL, group=NULL, palette="agSunset"){
  library(ggplot2)
  library(dplyr)
  library(colorspace)

  #error messaging
  #checking for data frame
  if(!("data.frame" %in% class(data))){
    stop("Data must be a data frame.")
  }

  #checking that x and y exist as variables
  if (!(x %in% names(data)) || !(y %in% names(data))) {
    stop("One or more of the variables you provided does not exist in the data frame.")
  }

  #removing NA values
  if (!is.null(color) && color %in% names(data)) {
    data <- data %>%
      filter(!is.na(.data[[x]]) & !is.na(.data[[y]]) & !is.na(.data[[color]]))
  }
  if (!is.null(group)) {
    data <- data %>%
      filter(!is.na(.data[[x]]) & !is.na(.data[[y]]) & !is.na(.data[[group]]))
  }
  else {
    data <- data %>%
      filter(!is.na(.data[[x]]) & !is.na(.data[[y]]))
  }

  #base plot
  plot <- ggplot(data, aes(x=.data[[x]], y=.data[[y]]))+
    geom_point(size=size, alpha=alpha)+
    theme_bw()+
    labs(x = x, y = y, title = paste(x, "against", y))

  #adding color, if color is a variable
  if (!is.null(color) && color %in% names(data)){
    plot <- plot +
      geom_point(aes(color=.data[[color]]), size=size, alpha=alpha)+
      scale_color_discrete_sequential(palette=palette)
  }

  #adding color, if color is not a variable
  if (!is.null(color) && !(color %in% names(data))){
    plot <- plot +
      geom_point(color = color, size=size, alpha=alpha)
  }

  #adding group, if group variable is supplied
  if (!is.null(group)) {
    plot <- plot + facet_wrap(as.formula(paste("~", group)), labeller = label_both)
  }

  #returning final plot
  return(plot)
}



