test_that("Testing findNearCells", {
  data("spe_test")

  spe <- readHoodData(spe, anno_col = "celltypes")

  fnc <- findNearCells(spe, k = 100)

  expect_true(is.list(fnc))

  expect_equal(length(fnc), 2)

  expect_equal(dim(fnc[[1]]), dim(fnc[[2]]))

  fnc2 <- findNearCells(spe, k = 100, reportDist = FALSE)

  expect_true(is.data.frame(fnc2))
})
