#'@title Astronomical Scatterplots
#'@description Create scatterplots for astronomy data, optimized for a large number of points
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns scatter plot with both variables
#'@import ggplot2
#'@import patchwork
#'@import colorspace
#'@examples
#'astroScatter(starcatalog, vmag, bv_color)
#'astroScatter(starcatalog, vmag, bv_color, size=0.2, alpha=0.7)


#add option for color variable, group variable, facet_wrap, etc.
astroScatter <- function(data, x, y, size=0.5, alpha=0.5){
  library(ggplot2)

  #removing NA values
  data <- data %>%
    filter(!is.na({{x}}) & !is.na({{y}}))

  #creating labels
  xname <- as.character(substitute(x))
  yname <- as.character(substitute(y))
  #figure out how to make the underscore things into subscripts maybe

  #making the plot
  ggplot(data)+
    geom_point(aes(x={{x}}, y={{y}}), size=size, alpha=alpha)+
    theme_bw()+
    labs(x = xname, y = yname, title = paste(xname, "against", yname))
}

