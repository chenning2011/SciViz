#'@title Correlation Heat map with Dendrogram
#'@description Calculates Pearson Correlation from a Matrix and plots resulting correlation values on a heatmap
#'@param matrix a matrix
#'@param color a vector of three colors. Default is blue, white, and red.
#'@returns a heatmap graph
#'@export
#'@import corrplot
#'@import gplots
#'@examples corrheatmap(mtcars)

random_matrix_uniform <- matrix(runif(20 * 20), nrow = 20, ncol = 20)
library(corrplot)
#convert to square matrix is.corr=FALSE
c <- cor(random_matrix_uniform)
color <- colorRampPalette(c("blue", "white", "red")) (20)
heatmap(x=c, col=colorz, symm = TRUE)

library(gplots)

corrheatmap <- function(matrix, color=c("blue", "white", "red")){
  c <- cor(matrix)
  color <- colorRampPalette(c("blue", "white", "red")) (20)
  heatmap.2(c,
            col = color,
            na.rm=TRUE,

            dendrogram = "both",

            key = TRUE,
            symm = TRUE,
            margins = c(10, 10),  # Adjust margins to fit labels
            cexRow = 1,           # Adjust row label size
            cexCol = 1,           # Adjust column label size
            las = 3,              # Angles column labels
            trace = "none")       # Turn off tracing lines
}
corrheatmap(random_matrix_uniform)
corrheatmap(mtcars)
