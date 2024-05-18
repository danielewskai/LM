# lattice.gen <- function(n_row = 5, n_col = 5, down_spins = 0) {
#   d <- n_col * n_row
#   if (down_spins > d) # idk czy dopisywac jakies warunki typu 'argumenty musza byc naturalne' czy cos
#     {
#       stop("number of down-spins must be smaller than product of dimensions")
#     }
#   v <- rep(1, d)
#   d_cords <- sample(1:d, down_spins, replace = FALSE)
#   v[d_cords] <- -1
#   return(matrix(v, n_row, n_col))
# }
# my potrzebujemy grafowych krat a nie dla isinga bezpośrednio

lattice.gen <- function(n_row = 3, n_col = 3) {
  n <- n_row*n_col
  nr_ver <- 1:n
  edges <- list()
  for (j in 1:n_col){
    for (i in 1:(n_row-1)){
      edges[[length(edges) + 1]] <- c(j+(i-1)*n_col, j+i*n_col)
    }
  }

  for (i in 1:n_row){
    for (j in 1:(n_col-1)){
      edges[[length(edges) + 1]] <- c(j+(i-1)*n_col, j+(i-1)*n_col+1)
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

#krata <- lattice.gen(7, 10, 32)
krata <- lattice.gen(7, 10)
krata$edges
krata$nei_matrix
