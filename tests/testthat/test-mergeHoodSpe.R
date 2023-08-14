test_that("Testing mergeHoodSpe", {
  data("spe_test")
  spe <- readHoodData(spe, anno_col = "celltypes")
  fnc <- findNearCells(spe, k = 100)
  pm <- scanHoods(fnc$distance)
  pm2 <- mergeByGroup(pm, fnc$cells)

  expect_false(all(colnames(pm2) %in% colnames(colData(spe))))

  spe <- mergeHoodSpe(spe, pm2)

  expect_true(all(colnames(pm2) %in% colnames(colData(spe))))
})
