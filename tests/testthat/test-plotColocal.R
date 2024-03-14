test_that("Testing plotColocal", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)
  
  p <- plotColocal(spe, pm_cols = colnames(pm2))
  
  expect_s4_class(p, "HeatmapList")
  
  p <- plotColocal(spe, pm_cols = colnames(pm2), self_cor = FALSE, by_group = "cell_annotation")
  expect_s4_class(p, "HeatmapList")

})
