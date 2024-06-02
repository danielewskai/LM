install.packages("plot.matrix")
library(plot.matrix)

M.lattice.gen <- function(n_row = 5, n_col = 5, down_spins = 0) {
   d <- n_col * n_row
   if (down_spins > d) # idk czy dopisywac jakies warunki typu 'argumenty musza byc naturalne' czy cos
     {
       stop("number of down-spins must be smaller than product of dimensions")
     }
   v <- rep(1, d)
   d_cords <- sample(1:d, down_spins, replace = FALSE)
   v[d_cords] <- -1
   return(matrix(v, n_row, n_col))
 }

M <- M.lattice.gen(10,10,60)

X <- matrix(M<0, nrow = 10)

plot(X)


plot(X, col = c("green", "violet"), axis.col = NULL, axis.row = NULL, key = NULL,
     border = NA, xlab = NA, ylab = NA, main = NA)

plot(X, col = c("darkblue", "lightpink"), axis.col = NULL, axis.row = NULL, key = NULL,
     border = NA, xlab = NA, ylab = NA, main = NA)

plot(X, col = c("darkslateblue", "mediumorchid"), axis.col = NULL, axis.row = NULL, key = NULL,
     border = NA, xlab = NA, ylab = NA, main = NA)

plot(X, col = c("mediumpurple3", "lightblue"), axis.col = NULL, axis.row = NULL, key = NULL,
     border = NA, xlab = NA, ylab = NA, main = NA)
