ising <- function(temp = 0.0001, graph = complete.graph.gen(5)) {
  n <- ncol(graph$nei_matrix)
  s_up <- rep(1,n)
  s_down <- rep(-1,n)
  stans <- list(s_up, s_down)
  U_vec <- c(runif(1))
  k <- 0
  memory <- list()
  while(k < 3) {
    U_vec <- append(U_vec, runif(2^(k-1))) # poprawić
    for (i in (2^k):1){
      print(i)
      for (s in stans){
        print(s)
        if (is.null(memory[[paste(as.character(-i),paste(as.character(s), collapse = ','), collapse = ";")]])){
          for (v in 1:n){
            print(v)
            stan <- s
            nei_plus <- neighbours(graph$nei_matrix[v,], stan)$nb_plus
            nei_minus <- neighbours(graph$nei_matrix[v,], stan)$nb_minus
            prog <- exp(2*temp*(nei_plus-nei_minus))/(exp(2*temp*(nei_plus-nei_minus))+1)
            if (U_vec[i]<prog) {
              stan[v] <- 1
            } else{
              stan[v] <- -1
            }
          }
          memory[[paste(as.character(-i),paste(as.character(s), collapse = ','), collapse = ";")]] <- stan
        }
      }
    }
    k <- k+1
    #memory[[paste(as.character(-1),paste(as.character(s_up), collapse = ','), collapse = ";")]]
  }
}

ncol(complete.graph.gen(5)$nei_matrix)
