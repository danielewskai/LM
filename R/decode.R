decode <- function(s){
  size <- ceiling(log2(s))
  v <- numeric(size)
  for(k in 1:size){
    v[k] <- s %% 2
    s <- floor(s/2)
  }
  return(v)
}
decode(25)
