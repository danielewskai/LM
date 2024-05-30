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

vertices <- 1:10
edges <- unlist(complete.graph.gen(10)$edges)
g <- graph(edges, directed=FALSE)
plot(g, vertex.color = (ising(0.01, complete.graph.gen(10), step = log2(20)))+1, vertex.size = 50)

