# Does PCA and returns the first two components from the X. When X is a 2D
# matrix, this effectively rotates (and potentially reflects) the point set
# so the data aligns along the PCs.
pc_rotate <- function(X) {
  X <- scale(X, center = TRUE, scale = FALSE)
  s <- svd(X, nu = 2, nv = 0)
  s$u %*% diag(c(s$d[1:2]))
}

prepare_coords <- function(
  coords,
  pc_axes = FALSE,
  xlim = NULL,
  ylim = NULL,
  equal_axes = FALSE
) {
  if (methods::is(coords, "list") && !is.null(coords$coords)) {
    coords <- coords$coords
  }
  if (methods::is(coords, "data.frame")) {
    coords <- as.matrix(coords)
  }
  if (!is.matrix(coords) || !is.numeric(coords)) {
    stop("'coords' must be a numeric matrix or data frame.", call. = FALSE)
  }
  if (nrow(coords) < 1 || ncol(coords) != 2) {
    stop(
      "'coords' must have at least one row and exactly two columns.",
      call. = FALSE
    )
  }
  if (any(!is.finite(coords))) {
    stop("'coords' must contain only finite values.", call. = FALSE)
  }
  if (pc_axes && nrow(coords) < 2) {
    stop("'pc_axes' requires at least two coordinate rows.", call. = FALSE)
  }
  if (!is.null(xlim)) {
    xlim <- validate_numeric_limits(xlim)
  }
  if (!is.null(ylim)) {
    ylim <- validate_numeric_limits(ylim)
  }

  if (pc_axes) {
    coords <- pc_rotate(coords)
  }
  if (equal_axes) {
    limits <- base::range(coords)
    xlim <- limits
    ylim <- limits
  }

  list(
    coords = coords,
    xlim = xlim,
    ylim = ylim,
    fixed_aspect = equal_axes
  )
}

validate_cex <- function(cex) {
  if (
    !is.numeric(cex) ||
      length(cex) != 1 ||
      is.na(cex) ||
      !is.finite(cex) ||
      cex <= 0
  ) {
    stop("'cex' must be a finite positive number.", call. = FALSE)
  }
}
