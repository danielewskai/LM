neighbours <- function(nei, state) {
    ct <- table(state[nei])
    if (is.na(ct['-1'])){
      ct['-1'] <- 0
    }
    if (is.na(ct['1'])){
      ct['1'] <- 0
    }
    list(nb_minus = ct['-1'], nb_plus = ct['1'])
}
