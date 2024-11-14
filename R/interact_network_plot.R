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

