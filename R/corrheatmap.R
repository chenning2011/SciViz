#'@title Correlation Heat Map with or Without Dendrogram
#'@description Calculates Pearson Correlation from a Matrix and plots resulting correlation values on a heatmap with color key, histogram, and optional dendrograms.
#'@param data a matrix or a data frame
#'@param color (optional) a vector of three colors. Default is cyan, magenta, and yellow.
#'@param title (optional) a character vector. Default is "Correlation Heatmap"
#'@param dend (optional) a string: "both","row","column","none". Default is both.
#'@returns A heatmap graph in a new quartz window.
#'@export
#'@import corrplot
#'@import gplots
#'@import grDevices
#'@examples
#'#Graph will open in a new Quartz window when the corrheatmap function is run
#'#Here are some examples of how to use the function
#'corrheatmap(mtcars, title="Correlations of Car Features", dend="row")
#'corrheatmap(mtcars, title="Correlations of Car Features", dend="col")
#'corrheatmap(mtcars, title="Correlations of Car Features", dend="both")
#'corrheatmap(mtcars, title="Correlations of Car Features", dend="none")


corrheatmap <- function(data, colors = c("#00ffff", "#ff00ff", "#ffff00"), n_colors=20, title = "Correlation Heatmap", dend="both") {

  #new environment because graph is too large for RStudio
  dev.new()

  #create correlation values
  c <- cor(data)

  #create color palette based on user input or default
  color <- colorRampPalette(colors) (n_colors)

  #create graph in new window
  heatmap.2(c,
            col = color,
            na.rm = TRUE,
            dendrogram = dend,
            key = TRUE,
            symm = TRUE,
            margins = c(10, 10),  # Adjust margins to fit labels
            cexRow = 1,           # Adjust row label size
            cexCol = 1,           # Adjust column label size
            las = 3,              # Angles column labels
            trace = "none",       # Turn off tracing lines
            main = title)         # Add the title
}
