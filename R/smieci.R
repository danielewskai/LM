propp.wilson <- function(st.m, N = 5) {
  u <- runif(2^N)
  v <- matrix(0, nrow(st.m), 2^N + 1)
  k <- 0
  while (T) {
    for (s in 1:nrow(st.m))
    {
      v[s, 2^k + 1] <- s
      for (l in (2^k):1)
      {
        v[s, l] <- update.fun(st.m, v[s, l + 1], u[l])
      }
    }
    if (max(v[, 1]) == min(v[, 1])) {
      return(v[1, 1])
    }
    k <- k + 1
    if (k > N) {
      return("dupa") # żeby algorytm sie nie mielił w nieskończoność
    }
  }
}
#
# M <- st.m.gen(15, 4)
# propp.wilson(M)
