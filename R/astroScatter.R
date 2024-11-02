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
#'a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
#'b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
#'c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
#'data <- rbind(a,b,c)
#'astroCornerplot(data, x, y)

astroScatter <- function(data, x, y){
  library(ggplot2)

  #just need to make a scatterplot that's optimized for a lot of things
}
