# Does PCA and returns the first two components from the X. When X is a 2D
# matrix, this effectively rotates (and potentially reflects) the point set
# so the data aligns along the PCs.
pc_rotate <- function(X) {
  X <- scale(X, center = TRUE, scale = FALSE)
  s <- svd(X, nu = 2, nv = 0)
  s$u %*% diag(c(s$d[1:2]))
}
