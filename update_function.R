update.fun <- function(st.m, state, u)
{
v <- st.m[state,]
s <- c(0,cumsum(v))
ans <- 0
for(k in 1:length(v)) # czy da sie to zrobic bez petli??
  {
    if(u >= s[k])
    {
      ans <- k
    }
}
return(ans)
}


c(0, cumsum(M[3,]))
MT <- st.m.gen(12, 3)
s <- update.fun(MT, 3, 0.5)
c(0, cumsum(MT[3,]))
