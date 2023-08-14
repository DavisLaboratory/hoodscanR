test_that("Testing plotProbDist", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)

  expect_silent(plotProbDist(spe, pm_cols = colnames(pm2)))

  spe <- clustByHood(spe, pm_cols = colnames(pm2), k = 5)

  expect_silent(plotProbDist(spe, pm_cols = colnames(pm2), by_cluster = TRUE, plot_all = TRUE))
})
