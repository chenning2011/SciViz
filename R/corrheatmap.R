#'@title Correlation Heat Map with or Without Dendrogram
#'@description Calculates Pearson Correlation from a Matrix and plots resulting correlation values on a heatmap with color key and histogram.
#'@param data a matrix or a data frame
#'@param color (optional) a vector of three colors. Default is blue, white, and red.
#'@param title (optional) a character vector. Default is "Correlation Heatmap"
#'@param dend (optional) a string: "both","row","column","none". Default is both.
#'@returns A heatmap graph in a new quartz window.
#'@export
#'@import corrplot
#'@import gplots
#'@examples corrheatmap(mtcars, title="Correlations of Car Features", dend="row")

corrheatmap <- function(data, colors = c("blue", "white", "red"), title = "Correlation Heatmap", dend="both") {
  #load necessary libraries
  library(corrplot)
  library(gplots)

  #new environment because graph is too large for RStudio
  dev.new()

  #create correlation values
  c <- cor(data)

  #create color palette based on user input or default
  color <- colorRampPalette(colors) (20)

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
