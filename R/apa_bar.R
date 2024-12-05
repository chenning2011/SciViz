#' @title Create APA-Style Bar Graph
#' @description Creates an APA-style bar graph with optional error bars, customizable titles,
#' labels, and a boxed legend title.
#' @param data A data frame containing the variables to be visualized.
#' @param x_var A character string naming the categorical variable for the x-axis.
#' @param y_var A character string naming the numeric variable for the y-axis.
#' @param group_var A character string naming the variable to group bars by (optional).
#' @param facet_var A character string naming the variable for faceting (optional).
#' @param error_var A character string naming the variable for error values (optional).
#' @param title A string for the plot title.
#' @param subtitle A string for the plot subtitle.
#' @param x_label A string for the x-axis label.
#' @param y_label A string for the y-axis label.
#' @param legend_title A string for the legend title.
#' @param tick_labels A vector of custom labels for x-axis ticks (optional).
#' @param bar_color A vector specifying colors for the bars (default is "black").
#' @param save_as A string specifying the file name to save the plot (optional).
#' @returns Returns a ggplot2 graph.
#' @export
#' @import ggplot2
#' @examples
#' # Example 1: Sample data for depression scores by treatment groups
#' barData1 <- data.frame(
#'   Group = rep(c("Control", "Treatment"), each = 5),
#'   Time = rep(c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"), times = 2),
#'   Depression_Scores = c(20, 22, 19, 21, 23, 25, 27, 26, 28, 30),
#'   Error_Values = c(2, 1, 3, 1, 2, 1, 2, 1, 2, 2)  # Example error values
#' )
#'
#' # Create the grouped bar graph with error bars, bold title, and italic subtitle
#' apa_bar(
#'   data = barData1,
#'   x_var = "Time",
#'   y_var = "Depression_Scores",
#'   group_var = "Group",
#'   error_var = "Error_Values",  # Specify the error variable
#'   title = "Depression Scores Over Time",
#'   subtitle = "Comparison Between Control and Treatment Groups",
#'   x_label = "Days",
#'   y_label = "Depression Scores",
#'   bar_color = c("black", "darkgrey"),  # Custom colors for groups
#'   legend_title = "Group"  # Adding legend title
#' )
#'
#' ## Example 2: Imbalanced Scenario
#' barData2 <- data.frame(
#'   Activity = rep(c("Running", "Swimming", "Cycling"), each = 4),
#'   Session_Type = rep(c("Weekday", "Weekend"), times = 6),
#'   Mean_Duration = c(30, 40, 25, 35, 45, 20, 50, 30, 55, 25, 60, 40),
#'   Error_Value = c(5, 7, 4, 6, 5, 3, 6, 4, 8, 5, 7, 3)  # Example error values
#' )
#'
#' # Create the grouped bar graph with error bars
#' apa_bar(
#'   data = barData2,
#'   x_var = "Activity",
#'   y_var = "Mean_Duration",
#'   group_var = "Session_Type",
#'   error_var = "Error_Value",  # Specify the error variable
#'   title = "Average Exercise Duration by Activity",
#'   subtitle = "Comparison of Weekday and Weekend Sessions",
#'   x_label = "Activity Type",
#'   y_label = "Mean Duration (minutes)",
#'   bar_color = c("black", "darkgrey")  # Different custom colors for groups
#' )
#' # Note: This example illustrates an imbalanced design, which may result in misleading visual representations.



apa_bar <- function(data, x_var, y_var, group_var = NULL, facet_var = NULL,
                    error_var = NULL, # New parameter for error values
                    title = "Title",
                    subtitle = "Subtitle",
                    x_label = "X-axis Label",
                    y_label = "Y-axis Label",
                    legend_title = "Legend Title",
                    tick_labels = NULL,
                    bar_color = "black",  # Default color
                    save_as = NULL) {

  # Create the base plot
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[group_var]])) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black",
             size = 0.5) +  # Black border for each bar
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         fill = legend_title) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Bold title
      plot.subtitle = element_text(size = 14, face = "italic"),  # Italic subtitle
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_text(size = 12, face = "bold",
                                  hjust = 0.5, # Center the legend title
                                  margin = margin(b = 5)), # Margin below title
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(color = "black", fill = "transparent") # Box around legend
    )

  # Set custom colors if a group variable is provided
  if (!is.null(group_var)) {
    p <- p + scale_fill_manual(values = bar_color)
  } else {
    p <- p + scale_fill_manual(values = c(bar_color))  # For a single bar
  }

  # Add error bars if an error variable is provided
  if (!is.null(error_var)) {
    p <- p + geom_errorbar(aes_string(ymin = paste(y_var, "-", error_var),
                                      ymax = paste(y_var, "+", error_var)),
                           position = position_dodge(0.9),
                           width = 0.25,
                           color = "black",
                           size = 0.5)  # Error bars with black color
  }

  # Set custom tick labels if provided
  if (!is.null(tick_labels)) {
    p <- p + scale_x_discrete(labels = tick_labels)
  }

  # Add faceting if a facet variable is provided
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }

  # Save the plot as a file if specified
  if (!is.null(save_as)) {
    ggsave(save_as, plot = p, width = 8, height = 6, dpi = 300)
  }

  # Print the plot
  print(p)
}
