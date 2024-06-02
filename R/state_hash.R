state_hash <- function(s) {
  sum(2^(0:(length(s) - 1)) * ifelse(s > 0, 1, 0))
}
