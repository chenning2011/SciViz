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
#'a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
#'b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
#'c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
#'data <- rbind(a,b,c)
#'astroCornerplot(data, x, y)

#add options for colors of histograms
#add options for color palette for 2d histogram
#add options for the bins for all of the histograms (maybe just keep it simple and they all have the same number of bins)
#second option for bins for the contour plot
#add in possibility of more than 2 combinations
#figure out how to include dataset in the package itself to provide user with examples
astroCornerplot <- function(data, varlist=names(data), contour=TRUE, fill="mediumpurple", palette="Inferno", bins=70, contourBins = 5){
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(colorspace)
  library(rlang)

  #only take numeric variables from supplied variables in dataframe
  numeric_data <-select_if(data[,varlist], is.numeric)
  varlist <- names(numeric_data)

  #empty list for all plots
  plot_list <- list()

  #generating list of all pairs of variables
  var_pairs <- combn(varlist, 2, simplify = FALSE)

  #creating 2d histograms for the pairs of variables
  for (pair in var_pairs) {
    #creating variables with 1st and 2nd variable in pair list
    var_x <- pair[[1]]
    var_y <- pair[[2]]

    #turning it into a symbol for plotting
    var_x <- sym(var_x)
    var_y <- sym(var_y)

    #creating graph
    plot_2d <- ggplot(data) +
      geom_bin2d(aes(x = !!var_x, y = !!var_y), bins=bins) +
      scale_fill_continuous_sequential(palette = palette, begin = 1, end = 0) +
      theme_bw()

    #contour option
    if (contour) {
      plot_2d <- plot_2d +
        geom_density_2d(aes(x = !!var_x, y = !!var_y), color = "gray", bins=5)
    }

    #appending to plot list
    plot_list[[paste(var_x, var_y)]] <- plot_2d
  }

  #creating histograms for all the variables
  for(var in varlist){
    #turning characters in symbol
    v <- sym(var)

    #using the symbol to create histograms
    a <- ggplot(numeric_data)+
      geom_histogram(aes(x=!!v), fill=fill, bins=bins)+
      theme_bw()
    plot_list[[var]] <- a
  }

  #placing all of the xvariables from each plot into a list
  x_vars <- sapply(plot_list, function(plot) {
    #creating ggplot_build objects to extract the name of the xvariable
    a_built <- ggplot_build(plot)
    x_var <- a_built$plot$layers[[1]]$mapping$x
    x_var_clean <- gsub("~", "", x_var[[2]])
    return(x_var_clean)
  })

  #sorting the plots by the x variables
  sorted_plots <- plot_list[order(x_vars)]

  #plotting them based on the sorted x variables
  wrap_plots(sorted_plots, ncol = length(varlist))
}



