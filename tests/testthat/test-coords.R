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

test_that("prepare_coords extracts, rotates, limits, and fixes aspect", {
  coords <- cbind(x = c(0, 2, 4), y = c(10, 20, 30))

  prepared <- vizier:::prepare_coords(
    list(coords = as.data.frame(coords)),
    xlim = c(-1, 5),
    ylim = c(9, 31)
  )
  expect_equal(prepared$coords, coords)
  expect_equal(prepared$xlim, c(-1, 5))
  expect_equal(prepared$ylim, c(9, 31))
  expect_false(prepared$fixed_aspect)

  rotated <- vizier:::prepare_coords(coords, pc_axes = TRUE)
  expect_equal(rotated$coords, vizier:::pc_rotate(coords))

  equal <- vizier:::prepare_coords(
    coords,
    xlim = c(-1, 5),
    ylim = c(9, 31),
    equal_axes = TRUE
  )
  expect_equal(equal$xlim, range(coords))
  expect_equal(equal$ylim, range(coords))
  expect_true(equal$fixed_aspect)
})

test_that("prepare_coords rejects unsupported coordinates and limits", {
  expect_error(
    vizier:::prepare_coords(matrix(1:3, ncol = 1)),
    "exactly two columns"
  )
  expect_error(
    vizier:::prepare_coords(cbind(1, Inf)),
    "only finite values"
  )
  expect_error(
    vizier:::prepare_coords(matrix(c(1, 2), nrow = 1), pc_axes = TRUE),
    "at least two"
  )
  expect_error(
    vizier:::prepare_coords(cbind(1:2, 3:4), xlim = c(2, 1)),
    "in increasing order"
  )
})
