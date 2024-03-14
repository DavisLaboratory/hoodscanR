test_that("Testing plotProbDist", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)
  
  p <- plotProbDist(spe, pm_cols = colnames(pm2))
  expect_s3_class(p, "ggplot")
  
  expect_error(plotProbDist(spe, pm_cols = c("xx","yy","zz")))

  spe <- clustByHood(spe, pm_cols = colnames(pm2), k = 5)

  p <- plotProbDist(spe, pm_cols = colnames(pm2), 
                    by_cluster = TRUE, plot_all = TRUE)
  expect_s3_class(p, "ggplot")
  
  p <- plotProbDist(spe, pm_cols = colnames(pm2), 
                    by_cluster = TRUE, plot_all = FALSE)
  expect_s3_class(p, "ggplot")
  
  expect_error(plotProbDist(spe, targetCells = "cell_xyz"))
})
