# Funkcja do obliczania energii
# Przyjmuje jako argumenty listę krawędzi i wektor konfiguracji spinów
# Zwraca wartość energii dla podanych danych
energy <- function(edges, state) {
  size <- length(edges)
  en <- 0
  for (i in 1:size) {
    en <- en + state[edges[[1]][1]]*state[edges[[i]][2]]
  }
  return(-en)
}

# Funkcja do zliczania liczby sąsiadów o spinie +1 oraz spinie -1 dla danego wierzchołka
# Przyjmuje jako argumenty wektor z macierzy sąsiedztwa, który wskazuje sąsiadów wybranego wierzchołka
# oraz wektor spinów dla danego grafu
# Zwraca listę zawierającą liczbę sąsiadów o spinie +1 oraz liczbę sąsiadów o spinie -1
neighbours <- function(nei, state) {
  tmp <- state[as.logical(nei)]
  nb_plus <- sum(tmp == 1)
  nb_minus <- length(tmp) - nb_plus
  list(nb_minus = nb_minus, nb_plus = nb_plus)
}

# Funkcja zmieniająca wektor spinów na liczbę dziesiętną, wykorzystująca modyfikację
# kodowania dzięstnego
# Przyjmuje wektor spinów
# Zwraca liczbę dziesiętną
state_hash <- function(s) {
  sum(2^(0:(length(s) - 1)) * ifelse(s > 0, 1, 0))
}

# Funkcja odkodowująca konfigurację spinów z systemu dziesiętnego
# Przyjmuje jako argumenty liczbę w systemie dziesiętnym oraz liczbę wierzchołków
# Zwraca wektor spinów
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
