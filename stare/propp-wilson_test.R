propp.wilson_a <- function(st.m, N = 5)
{
u <- runif(2^N)
v <- matrix(0, nrow(st.m), 2^N+1)
for(k in 0:N) #mozna zmienic na while true
  {
    for(s in 1:nrow(st.m))
    {
      v[s,2^k+1] <- s
      for(l in (2^k):1)
      {
        v[s,l] <- upd.fun.gen(st.m, v[s,l+1], u[l])
      }
    }
    if(max(v[,1]) == min(v[,1]))
      {
        return(v[1,1])
      }
  }
return(v)
}

M <- st.m.gen(20,3)

V <- propp.wilson(M,3)
