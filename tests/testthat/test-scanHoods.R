test_that("Testing scanHoods", {
  m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)

  pm <- scanHoods(m)

  expect_equal(nrow(pm),1000)

  expect_equal(ncol(pm), 100)

  expect_equal(rowSums(pm), rep(1, 1000))
})
