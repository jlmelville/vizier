test_that("turbo returns requested number of colors", {
  expect_length(turbo(1), 1)
  expect_length(turbo(10), 10)
  expect_match(turbo(10), "^#[0-9A-F]{6}$")
})

test_that("turbo can reverse colors", {
  expect_identical(turbo(10, rev = TRUE), rev(turbo(10)))
})

test_that("turbo validates n", {
  expect_error(turbo(0), "'n' must be a positive integer")
  expect_error(turbo(-1), "'n' must be a positive integer")
  expect_error(turbo(1.5), "'n' must be a positive integer")
  expect_error(turbo(Inf), "'n' must be a positive integer")
  expect_error(turbo(NA_real_), "'n' must be a positive integer")
  expect_error(turbo(c(1, 2)), "'n' must be a positive integer")
  expect_error(turbo("10"), "'n' must be a positive integer")
})

test_that("turbo validates start and end", {
  expect_error(
    turbo(10, start = 0, end = 0),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, start = -0.1),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, end = 1.1),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, start = NA_real_),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, end = Inf),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, start = c(0, 0.1)),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
  expect_error(
    turbo(10, start = "0"),
    "'start' and 'end' must be distinct finite values in \\[0, 1\\]"
  )
})
