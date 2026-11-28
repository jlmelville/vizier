library(vizier)

xy <- as.matrix(iris[1:24, 1:2])
edges <- data.frame(
  from = 1:12,
  to = 13:24,
  weight = seq(0, 1, length.out = 12)
)
spec <- list(edges = edges, route = "hammer", alpha = "weight")
serial <- prepare_overlay(xy, spec)
spec$n_threads <- 3L
parallel <- prepare_overlay(xy, spec)
stopifnot(
  identical(serial$paths, parallel$paths),
  identical(parallel$paths$edge_id, rep(seq_len(nrow(edges)), each = 50L))
)
