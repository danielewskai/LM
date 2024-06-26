# Funkcja służy do generowania grafów pełnych
# Przyjmuje argument n oznaczający liczbę wierzchołków w grafie
# Zwraca listę zawieracjącą listę par wierzchołków (czyli krawędzie) oraz macierz sąsiedztwa
complete.graph.gen <- function(n = 5) {

  if (!is.numeric(n) || n <= 0) {
    stop("Argument 'n' musi być liczbą całkowitą dodatnią")
  }

  edges <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      edges[[length(edges) + 1]] <- c(i, j)
    }
  }
  nei_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:length(edges)) {
    x <- edges[[i]][1]
    y <- edges[[i]][2]
    nei_matrix[x, y] <- 1
    nei_matrix[y, x] <- 1
  }
  return(list(edges = edges, nei_matrix = nei_matrix))
}
