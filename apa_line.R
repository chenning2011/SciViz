#' @title Create an APA-Style Line Chart with Error Bars
#' @description This function generates a line chart following APA 7th edition guidelines. It allows for the inclusion
#' of error bars to represent variability in the data.
#' @import ggplot2
#' @param data A data frame containing the data to be plotted.
#' @param x_var A string representing the name of the variable to be plotted on the x-axis.
#' @param y_var A string representing the name of the variable to be plotted on the y-axis.
#' @param group_var A string representing the name of the variable used for grouping lines. Default is NULL.
#' @param error_var A string representing the name of the variable for error values. Default is NULL.
#' @param title A string for the main title of the plot. Default is "Title".
#' @param subtitle A string for the subtitle of the plot. Default is "Subtitle".
#' @param x_label A string for the label of the x-axis. Default is "X-axis Label".
#' @param y_label A string for the label of the y-axis. Default is "Y-axis Label".
#' @param legend_title A string for the legend title. Default is "Legend Title".
#' @param save_as A string for the filename to save the plot. Default is NULL, which means the plot will not be saved.
#' @return A ggplot object representing the line chart.
#' @examples
# # Example data for the line chart
# example_data1 <- data.frame(
#   Time = rep(1:5, 3),
#   Value = c(10, 12, 15, 14, 18, 8, 9, 11, 13, 14, 20, 22, 21, 23, 24),
#   Group = rep(c("Group 1", "Group 2", "Group 3"), each = 5),
#   Error = c(1, 1, 2, 1, 2, 0.5, 1, 1.5, 1, 1, 1, 1, 1, 1, 1)  # Example error values
# )
#
# # Create the line chart with error bars and boxed legend title
# apa_line(
#   data = example_data1,
#   x_var = "Time",
#   y_var = "Value",
#   group_var = "Group",
#   error_var = "Error",
#   title = "Example Line Chart with Error Bars",
#   subtitle = "Following APA 7th Edition Guidelines",
#   x_label = "Time (Units)",
#   y_label = "Values"
# )
#
# # Sample data for monthly sales
# sales_data <- data.frame(
#   Month = rep(1:12, 2),
#   Sales = c(200, 250, 300, 350, 400, 450, 500, 480, 450, 400, 380, 350,
#             220, 230, 280, 320, 350, 370, 400, 420, 400, 390, 360, 340),
#   Store = rep(c("Store X", "Store Y"), each = 12),
#   Error = c(10, 15, 20, 25, 30, 20, 15, 10, 5, 10, 15, 20,
#             10, 10, 15, 20, 25, 15, 10, 5, 10, 15, 20, 15)  # Example error values
# )
#
# # Create the line chart for monthly sales
# apa_line(
#   data = sales_data,
#   x_var = "Month",
#   y_var = "Sales",
#   group_var = "Store",
#   error_var = "Error",
#   title = "Monthly Sales Comparison",
#   subtitle = "Sales Trends for Store X and Store Y",
#   x_label = "Month",
#   y_label = "Sales ($)",
#   legend_title = "Store"  # Adding legend title
# ) +
#   scale_x_continuous(breaks = seq(1, 12, by = 1),
#                      labels = seq(1, 12, by = 1)) +
#   theme(axis.ticks.x = element_line(color = "black"))



# Create an APA-style line chart with error bars
apa_line <- function(data, x_var, y_var, group_var = NULL,
                     error_var = NULL, # Parameter for error values
                     title = "Title",
                     subtitle = "Subtitle",
                     x_label = "X-axis Label",
                     y_label = "Y-axis Label",
                     legend_title = "Legend Title",
                     save_as = NULL) {

  library(ggplot2)

  # Create the base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var, color = group_var)) +
    geom_line(size = 1) +  # Line size
    geom_point(size = 3) +  # Points on the lines
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = legend_title) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_text(size = 12, face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(color = "black", fill = "transparent")
    )

  # Add error bars if an error variable is provided
  if (!is.null(error_var)) {
    p <- p + geom_errorbar(aes_string(ymin = paste(y_var, "-", error_var),
                                      ymax = paste(y_var, "+", error_var)),
                           width = 0.2,
                           color = "black",
                           size = 0.5)
  }

  # Save the plot as a file if specified
  if (!is.null(save_as)) {
    ggsave(save_as, plot = p, width = 8, height = 6, dpi = 300)
  }

  # Print the plot
  print(p)
}
