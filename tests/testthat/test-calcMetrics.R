test_that("Testing calcMetrics", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)
  spe <- mergeHoodSpe(spe, pm2)

  expect_false("entropy" %in% colnames(colData(spe)))
  expect_false("perplexity" %in% colnames(colData(spe)))

  spe <- calcMetrics(spe, pm_cols = colnames(pm2))

  expect_true("entropy" %in% colnames(colData(spe)))
  expect_true("perplexity" %in% colnames(colData(spe)))
  
  
  expect_error(calcMetrics(spe, pm_cols = c("xx","yy","zz")))
  
  spe1 <- calcMetrics(spe, pm = pm)
  
  expect_true("entropy" %in% colnames(colData(spe1)))
  expect_true("perplexity" %in% colnames(colData(spe1)))
})
