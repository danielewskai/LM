# Funkcja służy do generowania drzew
# Przyjmuje dwa argumenty: n - liczba wierzchołków oraz branching_factor - maksymalna
# liczba sąsiadów pewnego wierzchołka
# Zwraca listę zawieracjącą listę par wierzchołków (czyli krawędzie) oraz macierz sąsiedztwa
tree.gen <- function(n = 5, branching_factor = 2) {

  if (!is.numeric(n) || n <= 0) {
    stop("Argument 'n' musi być liczbą całkowitą dodatnią")
  }

  if (!is.numeric(branching_factor) || branching_factor <= 1 || branching_factor >= n) {
    stop("Argument 'branching_factor' musi być liczbą całkowitą większą niż 1 i mniejszą niż n")
  }

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
