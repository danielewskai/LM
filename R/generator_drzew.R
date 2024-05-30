tree.gen <- function(n = 5, branching_factor = 2) {
  edges <- list()
  nei_matrix <- matrix(0, nrow = n, ncol = n)
  add_edges <- function(v, w, branching_factor) {
    children <- 0
    nx <- sample.int(branching_factor - 1, size = 1)
    for (i in 1:nx) {
      if (w <= n) {
        edges[[length(edges) + 1]] <<- c(v, w)
        w <- w + 1
        children <- children + 1
      } else {
        break
      }
    }
    return(w)
  }
  w <- 2
  for (v in 1:(n - 1)) {
    w <- add_edges(v, w, branching_factor)
    if (w > n) {
      break
    }
  }
  for (i in 1:length(edges)) {
    x <- edges[[i]][1]
    y <- edges[[i]][2]
    nei_matrix[x, y] <- 1
    nei_matrix[y, x] <- 1
  }
  return(list(edges = edges, nei_matrix = nei_matrix))
}
