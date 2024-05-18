lattice.gen <- function(n_row = 5, n_col = 5, down_spins = 0)
{
d <- n_col*n_row
if(down_spins > d) #idk czy dopisywac jakies warunki typu 'argumenty musza byc naturalne' czy cos
  {
    stop("number of down-spins must be smaller than product of dimensions")
  }
v <- rep(1, d)
d_cords <- sample(1:d, down_spins, replace = FALSE)
v[d_cords] <- -1
return(matrix(v, n_row, n_col))
}

krata <- lattice.gen(7, 10, 32)
