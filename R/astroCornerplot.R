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
#'astroCornerplot(starcatalog, contour=FALSE, fill="mediumpurple", palette = "Purples", bins=40)

#option for page, true it's all together, false they're separate
#make sure someone who isn't in this field can understand how the graph works
astroCornerplot <- function(data, varlist=names(data), contour=TRUE, fill="mediumpurple", palette="Inferno", bins=70, contourBins = 5){
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(colorspace)
  library(rlang)

  #turning off warnings
  options(warn=-1)

  #only take numeric variables from supplied variables in dataframe
  numeric_data <-select_if(data[,varlist], is.numeric)
  varlist <- names(numeric_data)

  #error messaging up here
  if(length(varlist)<2){
    stop("You must provide at least 2 numeric variables.")
  }

  #need to get rid of variables with huge ranges
  # for(var in varlist){
  #   if (max(numeric_data[[var]])-min(numeric_data[[var]])>100000000){
  #     stop(cat("The range in variable", var, "is too large. Consider performing a log transformation."))
  #   }
  # }

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
    plot_2d <- ggplot(numeric_data) +
      geom_bin2d(aes(x = !!var_x, y = !!var_y), bins=bins) +
      scale_fill_continuous_sequential(palette = palette, begin = 1, end = 0) +
      theme_bw()

    #contour option
    if (contour) {
      plot_2d <- plot_2d +
        geom_density_2d(aes(x = !!var_x, y = !!var_y), color = "gray", bins=5)
    }

    #adding the plots into the list in the right order (hopefully)
    if (!is.null(plot_list[[as_label(var_x)]])) {
      #if there is something for the x variable already, adding in the new plot so that it's with the other items with that x variable
      plot_list[[as_label(var_x)]] <- c(plot_list[[as_label(var_x)]], list(plot_2d))
    } else {
      #creating new list w/just the plot we just made, adds it to position with label for the x variable
      plot_list[[as_label(var_x)]] <- list(plot_2d)
    }
  }

  #creating histograms for all the variables
  for(var in varlist){
    #turning characters in symbol
    v <- sym(var)

    #using the symbol to create histograms
    a <- ggplot(numeric_data)+
      geom_histogram(aes(x=!!v), fill=fill, bins=bins)+
      theme_bw()

    #adding to corresponding xvar column
    plot_list[[var]] <- c(list(a), plot_list[[var]])
  }

  #need to create empty spaces for the columns that have less than max number of vars
  #determining maximum it can be based on the length of each plot list
  max_height <-max(sapply(plot_list, length))

  #making empty plots to take up space
  blank_plot <- ggplot() + theme_void() # Blank ggplot for padding
  plot_list <- lapply(plot_list, function(col) {
    #add as many blank plots as i need (based on the diff. btwn max height and actual height)
    #then combine that with the plots that already exist
    c(rep(list(blank_plot), max_height - length(col)), col)
  })

  #going through the list, creating sublists for column of plots that can be combined
  plot_columns <- lapply(plot_list, wrap_plots, ncol=1)

  #arranging the columns with each of the sub lists, should have same number of columns as number of variables
  wrap_plots(plot_columns, ncol=length(varlist))
}

