neighbours <- function(nei, state) {
  tmp <- state[as.logical(nei)]
  nb_plus <- sum(tmp == 1)
  nb_minus <- length(tmp) - nb_plus
  list(nb_minus = nb_minus, nb_plus = nb_plus)
}
