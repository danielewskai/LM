decode <- function(s, n = 1){
  if(s == 0){
    return(rep(-1,n))
  }
  v <- numeric(n)
  for(k in 1:n){
    v[k] <- s %% 2
    s <- floor(s/2)
  }
  return(2*v-1)
}
decode(25)
