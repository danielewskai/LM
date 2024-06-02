pr.w <- function(st.m) # ten najlepszy
{
  v <- matrix(0, nrow(st.m), 2)
  u <- runif(2)
  k <- 0
  while (T) {
    v[, 2^k + 1] <- c(1:nrow(st.m))
    for (s in 1:nrow(st.m))
    {
      for (l in (2^k):1)
      {
        v[s, l] <- update.fun(st.m, v[s, l + 1], u[l])
      }
    }
    if (max(v[, 1]) == min(v[, 1])) {
      return(v[1, 1])
    }
    v_1 <- matrix(0, nrow(st.m), 2^(k + 1))
    u_1 <- runif(2^(k + 1))
    v <- matrix(c(v_1, v), nrow(st.m))
    u <- c(u_1, u)
    k <- k + 1
  }
}

# pr.w(M)
#
# st.m <- M
#
# X <- st.m.gen(1000, 3)
# pr.w(X)
# propp.wilson(X)
# m1 <- st.m.gen(10)
# m2 <- st.m.gen(10)
# matrix(c(m1, m2), 10)
#
# start.time <- Sys.time()
# pr.w(X)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
#
# start.time <- Sys.time()
# propp.wilson(X)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
