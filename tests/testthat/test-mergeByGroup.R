test_that("Testing mergeByGroup", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)

  expect_equal(ncol(pm2), 6)
  expect_equal(nrow(pm2), nrow(pm))
  expect_equal(as.vector(rowSums(pm2)), rep(1, nrow(pm)))
})
