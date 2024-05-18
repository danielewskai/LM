s <- 8
X <- st.m.gen(20000, 4)
times1 <- numeric(s)
times2 <- numeric(s)

for (k in 1:s)
{
  start.time <- Sys.time()
  a <- propp.wilson(X)
  end.time <- Sys.time()
  times1[k] <- as.numeric(end.time - start.time)

  start.time <- Sys.time()
  a <- pr.w(X)
  end.time <- Sys.time()
  times2[k] <- as.numeric(end.time - start.time)
}

start.time <- Sys.time()
a <- pr.w(X)
end.time <- Sys.time()
m <- as.numeric(end.time - start.time)

mean(times1)
mean(times2)

mean(times1[-c(which.max(times1), which.min(times1))])
mean(times2[-c(which.max(times2), which.min(times2))])
