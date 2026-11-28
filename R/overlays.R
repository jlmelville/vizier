#' Select Edges for an Embedding Plot
#'
#' Prepare an ordinary edge table for the `overlays` argument of
#' [embed_plot()], [embed_ggplot()] or [embed_plotly()].
#'
#' @param x A list with a ranked neighbor index matrix `idx` and optional
#'   matching distance matrix `dist`. Row and neighbor IDs refer to the same
#'   observations. Examples include a uwot metric block (`fit$nn[[1]]`) and
#'   a neighbor graph returned by rnndescent. Alternatively, supply a square,
#'   symmetric numeric sparse Matrix, such as a uwot `fit$fgraph`.
#' @param select For ranked neighbors, `"all"`, `"nearest"` (rank one), or
#'   `"furthest"` (the last retained input-space neighbor in each row).
#'   Sparse graphs support only `"all"` because directional ranks are lost.
#' @param k Optional positive integer. With `"all"`, keep up to the first
#'   `k` non-self neighbors per row. With `"furthest"`, take exactly rank
#'   `k`; every row must have that many neighbors.
#' @details Ranked indices are one-based and distances must be nondecreasing
#'   within each row. Every occurrence of self is removed by ID. Other IDs
#'   retain their first occurrence, including distinct neighbors tied at zero
#'   distance. uwot normally counts self in its requested neighbor count.
#' @return A data frame with integer columns `from` and `to` indexing rows of
#'   the original coordinates. Ranked input contributes directed edges with
#'   `rank` and, if supplied, `distance`. Sparse input contributes a single
#'   undirected edge per nonzero above the diagonal, with its value as
#'   `weight`. The `n_vertices` attribute records the input observation count.
#' @export
#' @examples
#' nn <- list(idx = rbind(c(1, 2, 3), c(2, 1, 3), c(3, 2, 1)))
#' edges <- embed_edges(nn, select = "nearest")
#' embed_plot(cbind(1:3, c(1, 3, 2)), colors = "navy", overlays = edges)
embed_edges <- function(x, select = "all", k = NULL) {
  select <- match.arg(select, c("all", "nearest", "furthest"))
  if (methods::is(x, "sparseMatrix")) {
    if (select != "all" || !is.null(k)) {
      stop(
        "Sparse graphs support only 'all' without a rank cutoff.",
        call. = FALSE
      )
    }
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop(
        "Package 'Matrix' is required for sparse graph input.",
        call. = FALSE
      )
    }
    if (
      !(methods::is(x, "dMatrix") || methods::is(x, "iMatrix")) ||
        nrow(x) != ncol(x) ||
        !Matrix::isSymmetric(x, tol = 0, checkDN = FALSE)
    ) {
      stop("Supply a square, symmetric numeric sparse matrix.", call. = FALSE)
    }
    upper <- Matrix::triu(Matrix::drop0(x), k = min(1L, ncol(x)))
    entries <- Matrix::summary(upper)
    edges <- data.frame(from = entries$i, to = entries$j, weight = entries$x)
    n <- nrow(x)
  } else {
    if (!is.list(x) || !is.matrix(x[["idx"]])) {
      stop(
        "Supply a ranked neighbor list with an 'idx' matrix or a sparse graph.",
        call. = FALSE
      )
    }
    idx <- x[["idx"]]
    n <- nrow(idx)
    validate_edge_ids(idx, n, "idx")
    edges <- ranked_edges(idx, x[["dist"]], select, k)
  }
  attr(edges, "n_vertices") <- n
  edges
}

validate_edge_ids <- function(ids, n, name) {
  if (
    !is.numeric(ids) ||
      is.complex(ids) ||
      any(!is.finite(ids)) ||
      any(ids != trunc(ids)) ||
      any(ids < 1 | ids > n)
  ) {
    stop(
      sprintf("'%s' must contain integer row IDs between 1 and %d.", name, n),
      call. = FALSE
    )
  }
}

ranked_edges <- function(idx, distance, select, k) {
  if (!is.null(k)) {
    if (
      select == "nearest" ||
        !is.numeric(k) ||
        length(k) != 1L ||
        !is.finite(k) ||
        k < 1 ||
        k != trunc(k)
    ) {
      stop(
        "'k' must be a positive integer for 'all' or 'furthest'.",
        call. = FALSE
      )
    }
  }
  if (
    !is.null(distance) &&
      (!is.numeric(distance) ||
        !identical(dim(distance), dim(idx)) ||
        any(!is.finite(distance)))
  ) {
    stop(
      "'dist' must be a finite numeric matrix matching 'idx'.",
      call. = FALSE
    )
  }
  rows <- seq_len(nrow(idx))
  columns <- lapply(rows, function(i) {
    if (!is.null(distance) && is.unsorted(distance[i, ])) {
      stop(sprintf("'dist' is not ranked in row %d.", i), call. = FALSE)
    }
    neighbors <- idx[i, ]
    which(neighbors != i & !duplicated(neighbors))
  })
  counts <- lengths(columns)
  if (select == "furthest" && !is.null(k) && any(counts < k)) {
    stop(
      sprintf(
        "Row %d has fewer than %s non-self neighbors.",
        which(counts < k)[1L],
        k
      ),
      call. = FALSE
    )
  }
  from <- rep(rows, counts)
  rank <- sequence(counts)
  keep <- switch(
    select,
    all = if (is.null(k)) rep(TRUE, length(rank)) else rank <= k,
    nearest = rank == 1L,
    furthest = rank == (if (is.null(k)) counts[from] else k)
  )
  positions <- cbind(from, as.integer(unlist(columns, use.names = FALSE)))
  positions <- positions[keep, , drop = FALSE]
  edges <- data.frame(
    from = from[keep],
    to = as.integer(idx[positions]),
    rank = rank[keep]
  )
  if (!is.null(distance)) {
    edges$distance <- distance[positions]
  }
  edges
}

overlay_pairs <- function(overlays, n, keep) {
  if (is.null(overlays)) {
    return(NULL)
  }
  if (!is.data.frame(overlays) || !all(c("from", "to") %in% names(overlays))) {
    stop(
      "'overlays' must be an edge table with 'from' and 'to' columns.",
      call. = FALSE
    )
  }
  vertices <- attr(overlays, "n_vertices", exact = TRUE)
  if (!is.null(vertices) && vertices != n) {
    stop(
      "The graph and coordinates must have the same observation count.",
      call. = FALSE
    )
  }
  from <- overlays$from
  to <- overlays$to
  if (!is.null(dim(from)) || !is.null(dim(to))) {
    stop("Edge endpoint columns must be vectors of row IDs.", call. = FALSE)
  }
  validate_edge_ids(from, n, "from")
  validate_edge_ids(to, n, "to")
  visible <- from != to & keep[from] & keep[to]
  from <- from[visible]
  to <- to[visible]
  pairs <- data.frame(from = pmin(from, to), to = pmax(from, to))
  first <- rep_len(TRUE, nrow(pairs))
  if (nrow(pairs) > 1L) {
    # Stable radix order puts the earliest input row first in each pair.
    sorted <- order(pairs$from, pairs$to, method = "radix")
    current <- sorted[-1L]
    previous <- sorted[-length(sorted)]
    first[current] <- pairs$from[current] != pairs$from[previous] |
      pairs$to[current] != pairs$to[previous]
  }
  pairs <- pairs[first, , drop = FALSE]
  pairs$edge_id <- which(visible)[first]
  pairs
}

overlay_segments <- function(pairs, coords) {
  data.frame(
    x = coords[pairs$from, 1],
    y = coords[pairs$from, 2],
    xend = coords[pairs$to, 1],
    yend = coords[pairs$to, 2],
    row.names = NULL
  )
}
