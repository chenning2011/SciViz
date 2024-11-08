#'@title Astronomical Scatterplots
#'@description Create scatterplots for astronomy data, optimized for a large number of points
#'@export
#'@param data a data frame
#'@param x string, a numeric variable
#'@param y string, a numeric variable
#'@param size (optional) size for the points on the graph. defaults to 0.5
#'@param alpha (optional) transparency for the points on the graph. defaults to 0.5
#'@param color (optional) string, color of the points. can supply a variable or the name of a color.
#'@returns scatter plot with both variables
#'@import ggplot2
#'@import dplyr
#'@import colorspace
#'@examples
#'astroScatter(starcatalog, vmag, bv_color)
#'astroScatter(starcatalog, vmag, bv_color, size=0.2, alpha=0.7)


#add option for color variable, group variable, facet_wrap, etc.
astroScatter <- function(data, x, y, size=0.5, alpha=0.5, color=NULL){
  library(ggplot2)
  library(dplyr)
  library(colorspace)

  #checking that x and y exist as variables
  if (!(x %in% names(data)) || !(y %in% names(data))) {
    stop("One or more of the variables you provided does not exist in the data frame.")
  }

  #removing NA values
  if (!is.null(color) && color %in% names(data)) {
    data <- data %>%
      filter(!is.na(.data[[x]]) & !is.na(.data[[y]]) & !is.na(.data[[color]]))
  } else {
    data <- data %>%
      filter(!is.na(.data[[x]]) & !is.na(.data[[y]]))
  }

  #if time and energy - create new labels with subscripts and superscripts

  #plot option 1
  if (!is.null(color) && color %in% names(data)){
    ggplot(data)+
      geom_point(aes(x=.data[[x]], y=.data[[y]], color=.data[[color]]), size=size, alpha=alpha)+
      theme_bw()+
      labs(x = x, y = y, title = paste(x, "against", y), color=color)+
      scale_color_discrete_sequential(palette="ag_Sunset")
  }

  #making the plot - option 2, if color is not a variable
  else {ggplot(data)+
    geom_point(aes(x=.data[[x]], y=.data[[y]]), size=size, alpha=alpha)+
    theme_bw()+
    labs(x = x, y = y, title = paste(x, "against", y))
  }
}

