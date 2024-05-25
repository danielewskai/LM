ising <- function(temp = 0.01, graph = lattice.gen(4, 4), step = 10000) {
  new_state <- function(state, U_vec_step) {
    n <- ncol(graph$nei_matrix)
    stan <- state
    v <- sample(1:n, 1)
    nei_plus <- neighbours(graph$nei_matrix[v, ], stan)$nb_plus
    nei_minus <- neighbours(graph$nei_matrix[v, ], stan)$nb_minus
    prog <- exp(2 * temp * (nei_plus - nei_minus)) / (exp(2 * temp * (nei_plus - nei_minus)) + 1)
    if (U_vec_step < prog) {
      stan[v] <- 1
    } else {
      stan[v] <- -1
    }
    return(stan)
  }

  n <- ncol(graph$nei_matrix)
  s_up <- rep(1, n)
  s_down <- rep(-1, n)
  stans <- list(s_up, s_down)
  U_vec <- c(runif(1))
  k <- 0
  memory <- list()
  while (k < step) {
    U_vec <- append(U_vec, runif(2^(k - 1))) # poprawić
    for (i in (2^k):1) {
       print(i)
      stans_new <- list()
      for (s in stans) {
        # print(s)
        s_str <- paste(as.character(-i), paste(as.character(s), collapse = ","), collapse = ";")
        memorized <- memory[[s_str]]
        if (is.null(memorized)) {
          stan <- new_state(s, U_vec[i])
          memory[[s_str]] <- stan
          stans_new[[length(stans_new) + 1]] <- stan
        } else {
          stans_new[[length(stans_new) + 1]] <- memorized
        }
      }
      stans <- stans_new
    }
    k <- k + 1
    if (all(stans[[1]] == stans[[2]])) {
      return(stans[[1]])
    }
    # memory[[paste(as.character(-1),paste(as.character(s_up), collapse = ','), collapse = ";")]]
  }
}

ncol(complete.graph.gen(5)$nei_matrix)
res <- ising(temp = 0.01, graph = lattice.gen(3,3))
data <- matrix(res, ncol = 3)
library(ggplot2)
x <- 1:3
y <- 1:3
data <- expand.grid(X=x, Y=y)
res <- list()
for (i in 1:10){
  res[[length(res)+1]] <- ising(temp = 0.01, graph = lattice.gen(3,3))

}
data$Z <- res[[7]]
data <- data.frame(data)
ggplot(data, aes(X, Y, fill= Z)) +
  geom_tile()
