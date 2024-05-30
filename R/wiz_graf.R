library(igraph)
edges <- c(1, 2, 2, 3, 3, 4, 4, 1, 2, 4)
g <- graph(edges, directed=FALSE)
plot(g, main="Simple Graph")

# Define vertices and edges
vertices <- c(1, 2, 3, 4)
edges <- c(1, 2, 2, 3, 3, 4, 4, 1, 2, 4)

# Create graph object
g <- graph(edges, directed=FALSE)

# Set vertex attributes
V(g)$name <- c("A", "B", "C", "D")
V(g)$color <- c("red", "green", "blue", "yellow")
V(g)$size <- c(15, 30, 45, 60)

# Set edge attributes
E(g)$color <- "gray"
E(g)$width <- 2

# Plot the graph
plot(g, vertex.label=V(g)$name, vertex.color=V(g)$color, vertex.size=V(g)$size,
     edge.color=E(g)$color, edge.width=E(g)$width, main="Graph with Attributes")

vertices <- 1:6
edges <- unlist(complete.graph.gen(6)$edges)
g <- graph(edges, directed=FALSE)
plot(g, vertex.color = (ising(0.01, complete.graph.gen(6))+1), vertex.size = 50, vertex.label = NA)

vertices <- 1:6
edges <- unlist(complete.graph.gen(6)$edges)
g <- graph(edges, directed=FALSE)
temp <- seq(0, 1, by = 0.01)
for (t in temp) {
  file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/tree_graph", t*100, ".png")
  png(file, width = 800, height = 600)
  plot(g, vertex.color = (ising(t, complete.graph.gen(6))+3), vertex.size = 50, vertex.label = NA)
  dev.off()
}

vertices <- 1:16
edges <- unlist(lattice.gen(4,4)$edges)
g <- graph(edges, directed=FALSE)
temp <- seq(0, 1, by = 0.01)
for (t in temp) {
  file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/lattice_graph", t*100, ".png")
  png(file, width = 800, height = 600)
  plot(g, vertex.color = (ising(t, lattice.gen(4,4))+3), vertex.size = 50, vertex.label = NA)
  dev.off()
}


