#'@title Astronomical Cornerplots
#'@description Create cornerplots for astronomy data for an unlimited number of paired variables.
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns corner plot
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@examples
#'a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
#'b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
#'c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
#'data <- rbind(a,b,c)
#'astroCornerplot(data, x, y)

astroCornerplot <- function(data, x, y){
  library(ggplot2)
  ggplot(data, aes(x={{x}}, y={{y}}) ) +
    geom_bin2d(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
}
