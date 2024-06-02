# Funkcja służy do generowania krat
# Przyjmuje dwa argumenty: n_row - liczba rzędów wierzchołków oraz n_col - liczba
# wierzchołków w każdym rzędzie
# Zwraca listę zawieracjącą listę par wierzchołków (czyli krawędzie) oraz macierz sąsiedztwa
lattice.gen <- function(n_row = 3, n_col = 3) {

  if (!is.numeric(n_row) || n_row <= 0) {
    stop("Argument 'n_row' musi być liczbą całkowitą dodatnią")
  }

  if (!is.numeric(n_col) || n_col <= 0) {
    stop("Argument 'n_col' musi być liczbą całkowitą dodatnią")
  }

  n <- n_row * n_col
  nr_ver <- 1:n
  edges <- list()
  for (j in 1:n_col) {
    for (i in 1:(n_row - 1)) {
      edges[[length(edges) + 1]] <- c(j + (i - 1) * n_col, j + i * n_col)
    }
  }

  for (i in 1:n_row) {
    for (j in 1:(n_col - 1)) {
      edges[[length(edges) + 1]] <- c(j + (i - 1) * n_col, j + (i - 1) * n_col + 1)
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
