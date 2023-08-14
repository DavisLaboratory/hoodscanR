test_that("Testing clustByHood", {
  m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)

  clust <- clustByHood(m, k = 3)

  expect_equal(class(clust), "kmeans")

  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)

  spe <- clustByHood(spe, pm_cols = colnames(pm2), k = 5)

  expect_true("clusters" %in% colnames(colData(spe)))

  expect_equal(typeof(colData(spe)$clusters), "character")
})
