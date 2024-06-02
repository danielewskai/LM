mc.sample <- function(st.m, starting.state, sample.size = 10) {
  v <- numeric(sample.size)
  v[1] <- sample(1:nrow(st.m), 1, prob = st.m[starting.state, ])
  for (k in 2:sample.size)
  {
    v[k] <- sample(1:nrow(st.m), 1, prob = st.m[v[k - 1], ])
  }
  return(v)
}

# b <- mc.sample(MT, 1, 100)
#
#
# seq(pr.w(Ma), lenght.out = 10)
