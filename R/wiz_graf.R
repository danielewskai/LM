# # Generowanie obrazków dla różnych grafów i różnych temperatur, każda kombinacja pięciokrotnie
# library(igraph)
#
# vertices <- 1:6
# edges <- unlist(complete.graph.gen(6)$edges)
# g <- graph(edges, directed=FALSE)
# temp <- c(0, 0.01, 0.1, 0.4, 0.5, 0.8)
# for (t in temp) {
#   for (i in 1:5) {
#     file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/complete_graph", t*100, i, ".png")
#     png(file, width = 800, height = 600)
#     plot(g, vertex.color = (ising(t, complete.graph.gen(6))+3), vertex.size = 50, vertex.label = NA)
#     dev.off()
#   }
# }
#
# vertices <- 1:16
# edges <- unlist(lattice.gen(4,4)$edges)
# g <- graph(edges, directed=FALSE)
# temp <- c(0, 0.01, 0.1, 0.4, 0.5, 0.8)
# for (t in temp) {
#   for (i in 1:5){
#     file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/lattice_graph", t*100, i, ".png")
#     png(file, width = 800, height = 600)
#     plot(g, vertex.color = (ising(t, lattice.gen(4,4))+3), vertex.size = 30, vertex.label = NA)
#     dev.off()
#   }
# }
#
# vertices <- 1:10
# edges <- unlist(tree.gen(10, 3)$edges)
# g <- graph(edges, directed=FALSE)
# temp <- c(0, 0.01, 0.1, 0.4, 0.5, 0.8)
# for (t in temp) {
#   for (i in 1:5){
#     file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/tree_graph", t*100, i, ".png")
#     png(file, width = 800, height = 600)
#     plot(g, vertex.color = (ising(t, tree.gen(10, 3))+3), vertex.size = 25, vertex.label = NA)
#     dev.off()
#   }
# }
#
# # Obliczenia związane z szybkością symulacji dla różnych wielkości grafów
# library("microbenchmark")
# result_complete <- list()
# for (n in 2:7) {
#   result_complete[[length(result_complete)+1]] <- microbenchmark(ising(0.4, complete.graph.gen(n)), times = 10, unit = "s")
# }
#
# result_tree <- list()
# for (n in 3:20) {
#   result_tree[[length(result_tree)+1]] <- microbenchmark(ising(0.4, tree.gen(n)), times = 10, unit = "s")
# }
#
# result_lattice <- list()
# for (i in 2:5) {
#   for (j in i:5){
#     result_lattice[[length(result_lattice)+1]] <- microbenchmark(ising(0.4, lattice.gen(i, j)), times = 10, unit = "s")
#   }
# }
#
# save(result_complete, file = "~/GitHub/LM/R/Wyniki czasowe/complete_graph.RData")
# save(result_tree, file = "~/GitHub/LM/R/Wyniki czasowe/tree_graph.RData")
# save(result_lattice, file = "~/GitHub/LM/R/Wyniki czasowe/lattice_graph.RData")
#
# # Obliczenia związane z szybkością symulacji dla różnych typów grafów ze względu na temperaturę
# temp <- c(0, 0.01, 0.1, 0.4, 0.8, 1)
# result_complete_temp <- list()
# for (t in temp) {
#   result_complete_temp[[length(result_complete_temp)+1]] <- microbenchmark(ising(t, complete.graph.gen(5)), times = 10, unit = "s")
# }
#
# result_tree_temp <- list()
# for (t in temp) {
#   result_tree_temp[[length(result_tree_temp)+1]] <- microbenchmark(ising(t, tree.gen(8)), times = 10, unit = "s")
# }
#
# result_lattice_temp <- list()
# for (t in temp) {
#   result_lattice_temp[[length(result_lattice_temp)+1]] <- microbenchmark(ising(t, lattice.gen(3, 3)), times = 10, unit = "s")
# }
#
# save(result_complete_temp, file = "~/GitHub/LM/R/Wyniki czasowe/complete_graph_temp.RData")
# save(result_tree_temp, file = "~/GitHub/LM/R/Wyniki czasowe/tree_graph_temp.RData")
# save(result_lattice_temp, file = "~/GitHub/LM/R/Wyniki czasowe/lattice_graph_temp.RData")
#
#
# # Generowanie gif-a temp = 0.3
# isi <- ising(temp = 0.3, lattice.gen(4,4), mem_ret = TRUE)
# isi$result
# history <- isi$history
# code <- state_hash(rep(1,16))
# path <- list()
# path[[length(path)+1]] <- rep(1,16)
# for (i in length(history):1){
#   next_s <- history[[i]][as.character(code)][[1]]
#   path[[length(path)+1]] <- next_s
#   code <- state_hash(next_s)
# }
#
# library(plot.matrix)
# for (i in 1:length(path)) {
#   file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek", i, ".png")
#   png(file, width = 800, height = 600)
#   plot(matrix(path[[i]]<0, ncol = 4),col = c("darkslateblue", "mediumorchid"), main = NA,axis.col = NULL, axis.row = NULL, key = NULL,
#        border = NA, xlab = NA, ylab = NA)
#   dev.off()
# }
#
# library(magick)
# frames <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek", 1:length(path), ".png")
# m <- image_read(frames)
# m <- image_animate(m, fps = 50)
# image_write(m, "~/GitHub/LM/R/Wygenerowane obrazki/Gif/movie3.gif")
#
#
# # Generowanie gif-a dla temp = 0
# isi <- ising(temp = 0, lattice.gen(4,4), mem_ret = TRUE)
# isi$result
# history <- isi$history
# code <- state_hash(rep(1,16))
# path <- list()
# path[[length(path)+1]] <- rep(1,16)
# for (i in length(history):1){
#   next_s <- history[[i]][as.character(code)][[1]]
#   path[[length(path)+1]] <- next_s
#   code <- state_hash(next_s)
# }
#
# library(plot.matrix)
# for (i in 1:length(path)) {
#   file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek0", i, ".png")
#   png(file, width = 800, height = 600)
#   plot(matrix(path[[i]]<0, ncol = 4),col = c("darkslateblue", "mediumorchid"), main = NA,axis.col = NULL, axis.row = NULL, key = NULL,
#        border = NA, xlab = NA, ylab = NA)
#   dev.off()
# }
#
# library(magick)
# # ograniczam liczbę obrazków do gif-a ze względu na małą możliwość obliczeniową :(
# frames <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek0", 1:513, ".png")
# m <- image_read(frames)
# m <- image_animate(m, fps = 50)
# image_write(m, "~/GitHub/LM/R/Wygenerowane obrazki/Gif/movie0.gif")
#
#
#
# # Generowanie gif-a dla temp = 0
# isi <- ising(temp = 0.8, lattice.gen(4,4), mem_ret = TRUE)
# isi$result # tylko -1 a zaczynamy z samych 1
# history <- isi$history
# code <- state_hash(rep(1,16))
# path <- list()
# path[[length(path)+1]] <- rep(1,16)
# for (i in length(history):1){
#   next_s <- history[[i]][as.character(code)][[1]]
#   path[[length(path)+1]] <- next_s
#   code <- state_hash(next_s)
# }
#
#
# library(plot.matrix)
# for (i in 1:length(path)) {
#   file <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek8", i, ".png")
#   png(file, width = 800, height = 600)
#   plot(matrix(path[[i]]<0, ncol = 4),col = c("darkslateblue", "mediumorchid"), main = NA,axis.col = NULL, axis.row = NULL, key = NULL,
#        border = NA, xlab = NA, ylab = NA)
#   dev.off()
# }
#
# library(magick)
# # ograniczam liczbę obrazków do gif-a ze względu na małą możliwość obliczeniową :(
# frames <- paste0("~/GitHub/LM/R/Wygenerowane obrazki/Gif/obrazek8", 1:length(path), ".png")
# m <- image_read(frames)
# m <- image_animate(m, fps = 50)
# image_write(m, "~/GitHub/LM/R/Wygenerowane obrazki/Gif/movie8.gif")
