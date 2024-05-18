energy <- function(grid, J = 1) {
  size <- nrow(grid)
  en <- 0
  for (i in 1:size) {
    for (j in 1:size) {
      spin <- grid[i, j]
      neighbors <- grid[(i %% size) + 1, j] + grid[(i - 2) %% size + 1, j] +
        grid[i, (j %% size) + 1] + grid[i, (j - 2) %% size + 1]
      en <- en - J * spin * neighbors
    }
  }

  return(en / 2)
}
