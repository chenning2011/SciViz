#'@title Interactive Network Plot
#'@description Create interactive network plot for a data frame of edge list format.
#'@export
#'@param data a data frame of edge list format. This means each row should specify a link between 2 nodes. There should be two columns; one for the inital point, and a second target point.
#'@param to column NUMBER of initial point variable. Default is 1
#'@param from column NUMBER of target point variable. Default is 2
#'@param linkColour color of edges, MUST be a common color for the whole graph. default is #666
#'@param nodeColour color of nodes, MUST be a common color for the whole graph. Default is #69b3a2
#'@param opacity opacity of nodes. 0=transparent. 1=no transparency. Default is .9
#'@param title default is "Network Graph"
#'@param subtitle default is "Exploring Interactions and Relationships"
#'@returns an interactive network graph
#'@import dplyr
#'@import igraph
#'@import networkD3
#'@import htmltools
#'@import htmlwidgets
#'@examples
#'data_test <- data.frame(
#'from=c("Neuron 1", "Neuron 1", "Neuron 2", "Neuron 3", "Neuron 5", "Neuron 4", "Neuron 5", "Neuron 7", "Neuron 6", "Neuron 8", "Neuron 9", "Neuron 11", "Neuron 10"),
#'to=c("Neuron 2", "Neuron 3", "Neuron 1", "Neuron 4", "Neuron 1", "Neuron 6", "Neuron 2", "Neuron 8", "Neuron 2", "Neuron 9", "Neuron 5", "Neuron 7", "Neuron 1"),
#'inital_test=c("A", "A", "B", "D", "C", "D", "E", "B", "C", "K", "E", "A", "M"),
#'target_test=c("B", "E", "F", "A", "C", "A", "B", "Z", "A", "C", "A", "B", "K")
#')
#'interact_network_plot(data_test)
#'interact_network_plot(data_test, initial_pt = 3, target_pt = 4)
#'

interact_network_plot <- function(data, initial_pt=1, target_pt=2, linkColour= "#666", nodeColour= "#69b3a2", opacity= 0.9, title="Network Graph", subtitle="Exploring Interactions and Relationships"){
  library(igraph)
  library(networkD3)
  library(htmltools)
  library(htmlwidgets)

  #col_number_to <- which(colnames(df) == target_pt)
  #col_number_from <- which(colnames(df) == initial_pt)

  #create network graph
  p <- simpleNetwork(data,
                     height="100px",
                     width="100px",
                     Source = initial_pt,                 # column number of source
                     Target = target_pt,                 # column number of target
                     linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                     charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                     fontSize = 14,               # size of the node names
                     fontFamily = "serif",       # font og node names
                     linkColour = linkColour,        # colour of edges, MUST be a common colour for the whole graph
                     nodeColour = nodeColour,     # colour of nodes, MUST be a common colour for the whole graph
                     opacity = opacity,              # opacity of nodes. 0=transparent. 1=no transparency
                     zoom = T                    # Can you zoom on the figure?

  )

  #add titles
  htmlwidgets::prependContent(p,
                              tags$div(
                                htmltools::tags$h3(title,
                                        style = "text-align: center; font-family: serif; color: #333;"),
                                htmltools::tags$h4(subtitle,
                                        style = "text-align: center; font-family: serif; color: #666;")
                              ))

}
#--------------------------------------------------
interact_network_plot <- function(data, initial_pt = 1, target_pt = 2, linkColour = "#666", nodeColour = "#69b3a2", opacity = 0.9, title = "Network Graph", subtitle = "Exploring Interactions and Relationships") {
  library(igraph)
  library(networkD3)
  library(htmltools)
  library(htmlwidgets)

  # Create the network graph with simpleNetwork
  p <- simpleNetwork(data,
                     height = "500px",  # Adjusted height to fit better
                     width = "70%",     # Make the plot take 70% of the container width
                     Source = initial_pt,  # column number of source
                     Target = target_pt,  # column number of target
                     linkDistance = 10,    # distance between nodes
                     charge = -900,        # strength of node repulsion
                     fontSize = 14,        # font size of node names
                     fontFamily = "serif", # font of node names
                     linkColour = linkColour,  # colour of edges
                     nodeColour = nodeColour,  # colour of nodes
                     opacity = opacity,   # opacity of nodes
                     zoom = TRUE           # enable zoom
  )

  # Create a legend data frame (each connection as a row)
  legend_data <- data.frame(
    from = data[[initial_pt]],   # Source column
    to = data[[target_pt]]      # Target column
  )

  # Create the legend items with arrows between nodes (source → target)
  legend_items <- lapply(1:nrow(legend_data), function(i) {
    tags$span(style = "font-size: 16px; color: #333;",
              legend_data$from[i], " → ", legend_data$to[i])
  })

  # Wrap the legend items in an unordered list
  legend <- tags$ul(
    style = "list-style-type: none; padding-left: 0; margin-left: 20px; max-height: 300px; overflow-y: auto;",
    lapply(legend_items, function(x) tags$li(style = "margin-bottom: 10px;", x))
  )

  # Add titles to the graph
  p <- htmlwidgets::prependContent(p,
                                   tags$div(
                                     htmltools::tags$h3(title, style = "text-align: center; font-family: serif; color: #333;"),
                                     htmltools::tags$h4(subtitle, style = "text-align: center; font-family: serif; color: #666;")
                                   ))

  # Combine the plot and the legend into the layout with `htmlwidgets`
  layout <- tags$section(
    style = "display: flex; justify-content: space-between; padding: 20px;",
    tags$article(
      style = "flex: 0.7; display: inline-block;",  # 70% for the plot
      p
    ),
    tags$aside(
      style = "flex: 0.3; display: inline-block;",  # 30% for the legend
      legend
    )
  )

  # Return the layout as an HTML object
  return(layout)
}
#--------------------------------------------
interact_network_plot <- function(data, initial_pt = 1, target_pt = 2, linkColour = "#666", nodeColour = "#69b3a2", opacity = 0.9, title = "Network Graph", subtitle = "Exploring Interactions and Relationships") {
  library(igraph)
  library(networkD3)
  library(htmltools)
  library(htmlwidgets)

  # Create a data frame for Links (edges)
  links <- data.frame(
    source = match(data[[initial_pt]], unique(c(data[[initial_pt]], data[[target_pt]]))) - 1,  # match to node index (0-based)
    target = match(data[[target_pt]], unique(c(data[[initial_pt]], data[[target_pt]]))) - 1,  # match to node index (0-based)
    value = rep(1, nrow(data))  # Optional: you can add a 'value' column if needed
  )

  # Create a data frame for Nodes (vertices)
  nodes <- data.frame(
    name = unique(c(data[[initial_pt]], data[[target_pt]]))  # Get unique nodes from both source and target
  )

  # Define color scale for nodes
  node_color_scale <- 'd3.scaleOrdinal(["#69b3a2"])'  # You can customize this color palette

  # Create the network graph with forceNetwork() to enable arrows
  p <- forceNetwork(Links = links,
                    Nodes = nodes,
                    Source = "source",    # Column name for source
                    Target = "target",    # Column name for target
                    NodeID = "name",      # Column for node identifier
                    opacity = opacity,    # Opacity of nodes
                    zoom = TRUE,          # Enable zoom
                    linkColour = linkColour,  # Color of the links
                    linkWidth = 1.5,      # Line thickness
                    arrows = TRUE,        # Enable arrows on the edges
                    charge = -900,        # Node charge (strength of repulsion)
                    linkDistance = 10,    # Distance between links
                    fontSize = 14,        # Font size of node names
                    fontFamily = "serif", # Font for node names
                    colourScale = node_color_scale  # Node color scale
  )

  # Create a legend data frame (each connection as a row)
  legend_data <- data.frame(
    from = data[[initial_pt]],   # Source column
    to = data[[target_pt]]      # Target column
  )

  # Create the legend items with arrows between nodes (source → target)
  legend_items <- lapply(1:nrow(legend_data), function(i) {
    tags$span(style = "font-size: 16px; color: #333;",
              legend_data$from[i], " → ", legend_data$to[i])
  })

  # Wrap the legend items in an unordered list
  legend <- tags$ul(
    style = "list-style-type: none; padding-left: 0; margin-left: 20px; max-height: 300px;",
    lapply(legend_items, function(x) tags$li(style = "margin-bottom: 10px;", x))
  )

  # Add titles to the graph
  p <- htmlwidgets::prependContent(p,
                                   tags$div(
                                     htmltools::tags$h3(title, style = "text-align: center; font-family: serif; color: #333;"),
                                     htmltools::tags$h4(subtitle, style = "text-align: center; font-family: serif; color: #666;")
                                   ))

  # Now wrap the graph and legend inside an htmlwidget using htmltools
  layout <- tags$div(
    style = "display: flex; justify-content: space-between; padding: 20px; overflow-y: auto; max-height: 600px;",  # Add scroll for entire container if content overflows
    tags$div(
      style = paste0("flex: 0.7; width:", graph_width, "px;"),  # Graph width adjusted
      p
    ),
    tags$div(
      style = "flex: 0.3; max-height: 300px; overflow-y: auto;",  # Legend area scrollable but inside the layout
      legend
    )
  )

  # Return the layout within the HTML widget container
  return(htmltools::browsable(layout))  # Ensure it renders correctly in a browser
}



#-----------------------------------------
interact_network_plot <- function(data, initial_pt = 1, target_pt = 2,
                                  linkColour = "#666", nodeColour = "#69b3a2",
                                  opacity = 0.9, title = "Network Graph",
                                  subtitle = "Exploring Interactions and Relationships",
                                  min_width = 400, max_width = 1000, width_factor = 25) {  # Add parameters for dynamic width
  library(igraph)
  library(networkD3)
  library(htmltools)
  library(htmlwidgets)

  # Create a data frame for Links (edges)
  links <- data.frame(
    source = match(data[[initial_pt]], unique(c(data[[initial_pt]], data[[target_pt]]))) - 1,  # match to node index (0-based)
    target = match(data[[target_pt]], unique(c(data[[initial_pt]], data[[target_pt]]))) - 1,  # match to node index (0-based)
    value = rep(1, nrow(data))  # Optional: you can add a 'value' column if needed
  )

  # Create a data frame for Nodes (vertices)
  nodes <- data.frame(
    name = unique(c(data[[initial_pt]], data[[target_pt]])),  # Get unique nodes from both source and target
    group = rep(1, length(unique(c(data[[initial_pt]], data[[target_pt]]))))  # Add group column (placeholder, set all to 1)
  )

  # Calculate dynamic graph width based on number of nodes
  num_nodes <- nrow(nodes)
  graph_width <- min(max(min_width, num_nodes * width_factor), max_width)

  # Define color scale for nodes
  node_color_scale <- 'd3.scaleOrdinal(["#69b3a2"])'  # You can customize this color palette

  # Create the network graph with forceNetwork() to enable arrows
  p <- forceNetwork(Links = links,
                    Nodes = nodes,
                    Source = "source",    # Column name for source
                    Target = "target",    # Column name for target
                    NodeID = "name",      # Column for node identifier
                    opacity = opacity,    # Opacity of nodes
                    zoom = TRUE,          # Enable zoom
                    linkColour = linkColour,  # Color of the links
                    linkWidth = 1.5,      # Line thickness
                    arrows = TRUE,        # Enable arrows on the edges
                    charge = -900,        # Node charge (strength of repulsion)
                    linkDistance = 10,    # Distance between links
                    fontSize = 14,        # Font size of node names
                    fontFamily = "serif", # Font for node names
                    colourScale = node_color_scale,  # Node color scale
                    Group = "group"  # Column name for grouping (needed for color scaling)
  )

  # Create a legend data frame (each connection as a row)
  legend_data <- data.frame(
    from = data[[initial_pt]],   # Source column
    to = data[[target_pt]]      # Target column
  )

  # Create the legend items with arrows between nodes (source → target)
  legend_items <- lapply(1:nrow(legend_data), function(i) {
    tags$span(style = "font-size: 16px; color: #333;",
              legend_data$from[i], " → ", legend_data$to[i])
  })

  # Wrap the legend items in an unordered list
  legend <- tags$ul(
    style = "list-style-type: none; padding-left: 0; margin-left: 20px; max-height: 300px;",
    lapply(legend_items, function(x) tags$li(style = "margin-bottom: 10px;", x))
  )

  # Add titles to the graph
  p <- htmlwidgets::prependContent(p,
                                   tags$div(
                                     htmltools::tags$h3(title, style = "text-align: center; font-family: serif; color: #333;"),
                                     htmltools::tags$h4(subtitle, style = "text-align: center; font-family: serif; color: #666;")
                                   ))

  # Now wrap the graph and legend inside an htmlwidget using htmltools
  layout <- tags$div(
    style = "display: flex; justify-content: space-between; padding: 20px; overflow-y: auto; max-height: 600px;",  # Add scroll for entire container if content overflows
    tags$div(
      style = paste0("flex: 0.7; width:", graph_width, "px;"),  # Graph width adjusted based on number of nodes
      p
    ),
    tags$div(
      style = "flex: 0.3; max-height: 300px; overflow-y: auto;",  # Legend area scrollable but inside the layout
      legend
    )
  )

  # Return the layout within the HTML widget container
  return(htmltools::browsable(layout))  # Ensure it renders correctly in a browser
}





interact_network_plot(data_test)
