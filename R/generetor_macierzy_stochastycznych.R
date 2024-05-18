st.m.gen <- function(dim = 5, r = 3) {
  v <- round(runif(dim^2, 0, 10), r)
  for (k in 1:dim)
  {
    s <- sum(v[((k - 1) * dim + 1):(k * dim)])
    for (l in ((k - 1) * dim + 1):(k * dim))
    {
      v[l] <- floor(v[l] / s * 10^r) / 10^r
    }
    v[k * dim] <- 1 - sum(v[((k - 1) * dim + 1):(k * dim - 1)])
  }
  return(t(matrix(v, dim, dim)))
}

M <- st.m.gen(10, 1)
cumsum(M[2, ])
