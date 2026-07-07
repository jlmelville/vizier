test_that("pc_rotate returns centered orthogonal scores", {
  coords <- cbind(
    x = c(0, 1, 4, 6),
    y = c(1, 3, 2, 8)
  )

  rotated <- vizier:::pc_rotate(coords)
  centered <- scale(coords, center = TRUE, scale = FALSE)
  score_crossprod <- crossprod(rotated)

  expect_equal(dim(rotated), c(nrow(coords), 2))
  expect_equal(unname(colMeans(rotated)), c(0, 0), tolerance = 1e-12)
  expect_equal(score_crossprod[1, 2], 0, tolerance = 1e-12)
  expect_equal(sum(rotated^2), sum(centered^2), tolerance = 1e-12)
})
