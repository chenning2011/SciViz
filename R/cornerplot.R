#'@title Cornerplots
#'@description Create cornerplots for numeric data using an unlimited number of paired variables. While you can use an unlimited number of variables, it is not recommended to use more than six numeric variables.
#'@export
#'@param data a data frame.
#'@param varlist character, list with all numeric variables to create the cornerplot and histograms with. defaults to all variables in the dataset.
#'@param contour (optional) logical, specify whether to overlay contour plot on 2d histogram, defaults to TRUE.
#'@param fill (optional) fill color for histograms, defaults to mediumpurple.
#'@param bins (optional) number of bins for all histograms, defaults to 70.
#'@param palette (optional) color palette for 2d histogram scale, defaults to Inferno. options are available through the `scale_fill_continuous_sequential` function in the colorspace package.
#'@param contourBins (optional) number of bins for contour plot, defaults to 5.
#'@returns corner plot with 2d histogram showing intersection of both variables and regular histogram for each variable. Please note that the function can take a while to load, especially if you are running it on more than five variables.
#'@import dplyr
#'@import ggplot2
#'@import patchwork
#'@import colorspace
#'@import rlang
#'@examples
#'cornerplot(starcatalog)
#'cornerplot(starcatalog, contour=FALSE, fill="mediumpurple", palette = "Purples", bins=40)
#'cornerplot(starcatalog, varlist = c("vmag", "bv_color"), palette = "Blues", fill="dodgerblue3", bins = 50, contourBins = 3)

#option for page, true it's all together, false they're separate
#make sure someone who isn't in this field can understand how the graph works
cornerplot <- function(data, varlist=names(data), contour=TRUE, fill="mediumpurple", palette="Inferno", bins=30, contourBins = 3){
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(colorspace)
  library(rlang)

  #error messaging up here
  if(length(varlist)<2){
    stop("Please provide at least 2 numeric variables.")
  }

  if(!("data.frame" %in% class(data))){
    stop("Data must be a data frame.")
  }

  #turning off warnings
  options(warn=-1)

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

  #creating chunks so there are no more than 3 columns on each page
  chunks <- split(plot_columns, ceiling(seq_along(plot_columns) / 3 ))

  #getting the number of pages
  total_pages <- length(chunks)

  #show each page, ask user to press enter if there is more than one page
  for (i in seq_along(chunks)) {
    print(wrap_plots(chunks[[i]], ncol = length(chunks[[i]])))
    if (total_pages > 1 && i < total_pages) {
      readline(prompt = "Press Enter to see the next page...")
    }
  }
}

