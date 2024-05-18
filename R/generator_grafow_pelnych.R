complete.graph.gen <- function(n=5) {
  edges <- list()
  for (i in 1:n){
    for (j in 1:n){
      edges[[length(edges)+1]] <- c(i, j)
    }
  }
  nei_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:length(edges)){
    x <- edges[[i]][1]
    y <- edges[[i]][2]
    nei_matrix[x,y] <- 1
    nei_matrix[y,x] <- 1
  }
  return(list(edges = edges,nei_matrix = nei_matrix))
}

graf <- complete.graph.gen(5)
graf$edges
graf$nei_matrix
