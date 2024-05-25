neighbours <- function(nei, state) {
  nb_minus <- 0
  nb_plus <- 0
  for (i in state[as.logical(nei)]){
    if (i==1){
      nb_plus <- nb_plus + 1
    } else{
      nb_minus <- nb_minus + 1
    }
  }
  list(nb_minus = nb_minus, nb_plus = nb_plus)
}
