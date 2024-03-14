test_that("testing plotHoodMat", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)
  
  
  p <- plotHoodMat(spe, pm_cols = colnames(pm2))
  expect_s4_class(p, "Heatmap")
  
  expect_error(plotHoodMat(spe, pm_cols = "xyz"))
})
